/*--------------------------------------------------------------------*/
/* heapmgrbase.c                                                      */
/*--------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "chunkbase.h"  /* Provides Chunk_T and span-based header API */

#define FALSE 0
#define TRUE  1

/* Minimum number of *payload* units to request on heap growth.
 * (The actual request adds 1 header unit on top.) */
enum { SYS_MIN_ALLOC_UNITS = 1024 };

/* Head of the free list (ordered by ascending address). */
static Chunk_T s_free_head = NULL;

/* Heap bounds: [s_heap_lo, s_heap_hi).
 * s_heap_hi moves forward whenever the heap grows. */
static void *s_heap_lo = NULL, *s_heap_hi = NULL;

/*--------------------------------------------------------------------*/
/* check_heap_validity
 *
 * Lightweight integrity checks for the entire heap and the free list.
 * This is a *basic* sanity check. Passing it does not prove correctness
 * of all invariants, but it helps catch common structural bugs.
 *
 * Returns TRUE (1) on success, FALSE (0) on failure.
 *
 * Checks performed:
 *  - Heap bounds are initialized.
 *  - Every physical block within [s_heap_lo, s_heap_hi) passes chunk_is_valid.
 *  - Each node in the free list is marked CHUNK_FREE and passes chunk_is_valid.
 *  - Adjacency/Free-List consistency: if a free block's physical successor
 *    is exactly its next free block, then they should have been coalesced
 *    already (report as "uncoalesced").
 */
#ifndef NDEBUG
static int
check_heap_validity(void)
{
    Chunk_T w;

    if (s_heap_lo == NULL) { fprintf(stderr, "Uninitialized heap start\n"); return FALSE; }
    if (s_heap_hi == NULL) { fprintf(stderr, "Uninitialized heap end\n");   return FALSE; }

    if (s_heap_lo == s_heap_hi) {
        if (s_free_head == NULL) return TRUE;
        fprintf(stderr, "Inconsistent empty heap\n");
        return FALSE;
    }

    /* Walk all physical blocks in address order. */
    for (w = (Chunk_T)s_heap_lo;
         w && w < (Chunk_T)s_heap_hi;
         w = chunk_get_adjacent(w, s_heap_lo, s_heap_hi)) {
        if (!chunk_is_valid(w, s_heap_lo, s_heap_hi)) return FALSE;
    }

    /* Walk the free list; ensure nodes are free and not trivially coalescible. */
    for (w = s_free_head; w; w = chunk_get_next_free(w)) {
        Chunk_T n;

        if (chunk_get_status(w) != CHUNK_FREE) {
            fprintf(stderr, "Non-free chunk in the free list\n");
            return FALSE;
        }
        if (!chunk_is_valid(w, s_heap_lo, s_heap_hi)) return FALSE;

        n = chunk_get_adjacent(w, s_heap_lo, s_heap_hi);
        if (n != NULL && n == chunk_get_next_free(w)) {
            fprintf(stderr, "Uncoalesced adjacent free chunks\n");
            return FALSE;
        }
    }
    return TRUE;
}
#endif /* NDEBUG */

/*--------------------------------------------------------------------*/
/* bytes_to_payload_units
 *
 * Convert a byte count to the number of *payload* units, rounding up
 * to the nearest multiple of CHUNK_UNIT. The result does not include
 * the header unit. */
static size_t
bytes_to_payload_units(size_t bytes)
{
    return (bytes + (CHUNK_UNIT - 1)) / CHUNK_UNIT;
}

/*--------------------------------------------------------------------*/
/* header_from_payload
 *
 * Map a client data pointer back to its block header pointer by
 * stepping one header unit backward. */
static Chunk_T
header_from_payload(void *p)
{
    return (Chunk_T)((char *)p - CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* heap_bootstrap
 *
 * Initialize heap bounds using sbrk(0). Must be called exactly once
 * before any allocation occurs. Exits the process on fatal failure. */
static void
heap_bootstrap(void)
{
    s_heap_lo = s_heap_hi = sbrk(0);
    if (s_heap_lo == (void *)-1) {
        fprintf(stderr, "sbrk(0) failed\n");
        exit(-1);
    }
}

/*--------------------------------------------------------------------*/
/* coalesce_two
 *
 * Given two *adjacent* free blocks a and b (a < b), merge them into a.
 * The new span of 'a' becomes span(a) + span(b). The free-list link of
 * 'a' is updated to skip 'b'. Returns the merged block 'a'. */
static Chunk_T
coalesce_two(Chunk_T a, Chunk_T b)
{
    assert(a < b);
    assert(chunk_get_adjacent(a, s_heap_lo, s_heap_hi) == b);
    assert(chunk_get_status(a) == CHUNK_FREE);
    assert(chunk_get_status(b) == CHUNK_FREE);

    chunk_set_span_units(a, chunk_get_span_units(a) + chunk_get_span_units(b));
    chunk_set_next_free(a, chunk_get_next_free(b));
    return a;
}

/*--------------------------------------------------------------------*/
/* split_for_alloc
 *
 * Split a free block 'c' into:
 *   - a smaller *leading* free block of span 'remain_span'
 *   - a *trailing* allocated block 'alloc' of span 'alloc_span'
 *
 * Inputs:
 *   c          : free block to split
 *   need_units : required *payload* units (does not include header)
 *
 * Computations:
 *   alloc_span  = 1 (header) + need_units
 *   remain_span = span(c) - alloc_span
 *
 * Pre-conditions:
 *   remain_span > 0 (i.e., the split leaves a positive-size free block).
 *
 * Returns the header pointer of the newly created allocated block. */
static Chunk_T
split_for_alloc(Chunk_T c, size_t need_units)
{
    Chunk_T alloc;
    int old_span    = chunk_get_span_units(c);
    int alloc_span  = (int)(1 + need_units);
    int remain_span = old_span - alloc_span;

    assert(c >= (Chunk_T)s_heap_lo && c <= (Chunk_T)s_heap_hi);
    assert(chunk_get_status(c) == CHUNK_FREE);
    assert(remain_span > 0);

    /* Shrink the leading free block. */
    chunk_set_span_units(c, remain_span);

    /* The allocated block begins immediately after the smaller free block. */
    alloc = chunk_get_adjacent(c, s_heap_lo, s_heap_hi);
    chunk_set_span_units(alloc, alloc_span);
    chunk_set_status(alloc, CHUNK_USED);
    chunk_set_next_free(alloc, chunk_get_next_free(c));  /* harmless for allocated */

    return alloc;
}

/*--------------------------------------------------------------------*/
/* freelist_push_front
 *
 * Insert a block 'c' at the head of the free list (address-ordered).
 * If the new head is physically adjacent to the previous head,
 * coalesce them immediately to reduce fragmentation. */
static void
freelist_push_front(Chunk_T c)
{
    assert(chunk_get_span_units(c) >= 1);
    chunk_set_status(c, CHUNK_FREE);

    if (s_free_head == NULL) {
        s_free_head = c;
        chunk_set_next_free(c, NULL);
    }
    else {
        assert(c < s_free_head);
        chunk_set_next_free(c, s_free_head);
        if (chunk_get_adjacent(c, s_heap_lo, s_heap_hi) == s_free_head)
            coalesce_two(c, s_free_head);
        s_free_head = c;
    }
}

/*--------------------------------------------------------------------*/
/* freelist_insert_after
 *
 * Insert block 'c' into the free list *after* node 'e'. Then, if
 * 'c' is physically adjacent to its neighbors ('e' or the physical
 * successor), coalesce accordingly. Returns the final (possibly merged)
 * block that occupies 'c''s position. */
static Chunk_T
freelist_insert_after(Chunk_T e, Chunk_T c)
{
    Chunk_T n;

    assert(e < c);
    assert(chunk_get_status(e) == CHUNK_FREE);
    assert(chunk_get_status(c) != CHUNK_FREE);

    chunk_set_next_free(c, chunk_get_next_free(e));
    chunk_set_next_free(e, c);
    chunk_set_status(c, CHUNK_FREE);

    /* Merge with lower neighbor if adjacent. */
    if (chunk_get_adjacent(e, s_heap_lo, s_heap_hi) == c)
        c = coalesce_two(e, c);

    /* Merge with upper neighbor if that one is free and adjacent. */
    n = chunk_get_adjacent(c, s_heap_lo, s_heap_hi);
    if (n != NULL && chunk_get_status(n) == CHUNK_FREE)
        c = coalesce_two(c, n);

    return c;
}

/*--------------------------------------------------------------------*/
/* freelist_detach
 *
 * Remove block 'c' from the free list. If 'c' is the head, 'prev'
 * must be NULL; otherwise 'prev' must precede 'c' in the list.
 * The block is marked as CHUNK_USED afterwards. */
static void
freelist_detach(Chunk_T prev, Chunk_T c)
{
    assert(chunk_get_status(c) == CHUNK_FREE);

    if (prev == NULL)
        s_free_head = chunk_get_next_free(c);
    else
        chunk_set_next_free(prev, chunk_get_next_free(c));

    chunk_set_next_free(c, NULL);
    chunk_set_status(c, CHUNK_USED);
}

/*--------------------------------------------------------------------*/
/* sys_grow_and_link
 *
 * Request more memory from the system via sbrk() and link the new
 * block into the free list (coalescing when possible).
 *
 * Inputs:
 *   prev       : last node in the free list (or NULL if list is empty)
 *   need_units : required *payload* units
 *
 * Actions:
 *   - Compute grow_span = 1 + max(need_units, SYS_MIN_ALLOC_UNITS).
 *   - sbrk(grow_span * CHUNK_UNIT) to obtain one big block.
 *   - Temporarily mark it USED (to avoid free-list invariants while
 *     inserting) and then insert/merge into the free list. */
static Chunk_T
sys_grow_and_link(Chunk_T prev, size_t need_units)
{
    Chunk_T c;
    size_t grow_data = (need_units < SYS_MIN_ALLOC_UNITS) ? SYS_MIN_ALLOC_UNITS : need_units;
    size_t grow_span = 1 + grow_data;  /* header + payload units */

    c = (Chunk_T)sbrk(grow_span * CHUNK_UNIT);
    if (c == (Chunk_T)-1)
        return NULL;

    s_heap_hi = sbrk(0);

    chunk_set_span_units(c, (int)grow_span);
    chunk_set_next_free(c, NULL);
    chunk_set_status(c, CHUNK_USED);   /* will flip to FREE once inserted */

    if (s_free_head == NULL)
        freelist_push_front(c);
    else
        c = freelist_insert_after(prev, c);

    assert(check_heap_validity());
    return c;
}

/*--------------------------------------------------------------------*/
/* heapmgr_malloc
 *
 * Allocate a block capable of holding 'size' bytes. Zero bytes returns
 * NULL. The allocated region is *uninitialized*. Strategy:
 *  1) Convert 'size' to payload units (no header).
 *  2) First-fit search the free list for a block whose (span-1) >= need.
 *  3) If found:
 *       - split if larger than needed; otherwise detach as exact fit.
 *     Else:
 *       - grow the heap and repeat the same split/detach logic.
 *  4) Return the payload pointer (header + 1 unit). */
void *
heapmgr_malloc(size_t size)
{
    static int booted = FALSE;
    Chunk_T cur, prev, prevprev;
    size_t need_units;

    if (size == 0)
        return NULL;

    if (!booted) { heap_bootstrap(); booted = TRUE; }

    assert(check_heap_validity());

    need_units = bytes_to_payload_units(size);
    prevprev = NULL;
    prev = NULL;

    /* First-fit scan: usable payload units = span - 1 (exclude header). */
    for (cur = s_free_head; cur != NULL; cur = chunk_get_next_free(cur)) {
        size_t cur_payload = (size_t)chunk_get_span_units(cur) - 1;

        if (cur_payload >= need_units) {
            if (cur_payload > need_units)
                cur = split_for_alloc(cur, need_units);
            else
                freelist_detach(prev, cur);

            assert(check_heap_validity());
            return (void *)((char *)cur + CHUNK_UNIT);
        }
        prevprev = prev;
        prev = cur;
    }

    /* Need to grow the heap. */
    cur = sys_grow_and_link(prev, need_units);
    if (cur == NULL) {
        assert(check_heap_validity());
        return NULL;
    }

    /* If the new block merged with 'prev', back up one step. */
    if (cur == prev) prev = prevprev;

    /* Final split/detach on the grown block. */
    if ((size_t)chunk_get_span_units(cur) - 1 > need_units)
        cur = split_for_alloc(cur, need_units);
    else
        freelist_detach(prev, cur);

    assert(check_heap_validity());
    return (void *)((char *)cur + CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* heapmgr_free
 *
 * Free a previously allocated block pointed to by 'p'. If 'p' is NULL,
 * do nothing. The pointer must have been returned by heapmgr_malloc().
 * Strategy:
 *  1) Validate heap structure (debug only).
 *  2) Map payload pointer to its header.
 *  3) Find the insertion point in the address-ordered free list.
 *  4) Insert the block and coalesce with adjacent free neighbors. */
void
heapmgr_free(void *p)
{
    Chunk_T c, it, prev;

    if (p == NULL)
        return;

    assert(check_heap_validity());

    c = header_from_payload(p);
    assert(chunk_get_status(c) != CHUNK_FREE);

    /* Find address-ordered insertion point. */
    prev = NULL;
    for (it = s_free_head; it != NULL; it = chunk_get_next_free(it)) {
        if (c < it) break;
        prev = it;
    }

    /* Insert and coalesce. */
    if (prev == NULL)
        freelist_push_front(c);
    else
        (void)freelist_insert_after(prev, c);

    assert(check_heap_validity());
}
