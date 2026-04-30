/*--------------------------------------------------------------------*/
/* heapmgr1.c                                                         */
/* Heap manager using a doubly-linked free list with boundary tags.   */
/*                                                                    */
/* Key design decisions:                                               */
/* - Header (1 unit) + Footer (1 unit) per block = 2 units overhead   */
/* - Doubly-linked free list for O(1) removal                          */
/* - LIFO (front) insertion for O(1) free                              */
/* - Boundary tags enable O(1) coalescing with both neighbors          */
/* - First-fit allocation strategy                                     */
/* - Minimum block span = 3 (header + 1 payload + footer)              */
/*--------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

#include "chunk.h"

/*--------------------------------------------------------------------*/
/* Boolean constants used for readability.                             */
/*--------------------------------------------------------------------*/
#define FALSE 0
#define TRUE  1

/*--------------------------------------------------------------------*/
/* OVERHEAD_UNITS: number of units used by header + footer per block.  */
/*--------------------------------------------------------------------*/
enum { OVERHEAD_UNITS = 2 };

/*--------------------------------------------------------------------*/
/* MIN_GROW_UNITS: minimum payload units to request via sbrk.         */
/*--------------------------------------------------------------------*/
enum { MIN_GROW_UNITS = 1024 };

/*--------------------------------------------------------------------*/
/* Module state: free-list head and heap address bounds.               */
/*--------------------------------------------------------------------*/
static Chunk_T s_free_head = NULL;
static void *s_heap_lo = NULL;
static void *s_heap_hi = NULL;

/*--------------------------------------------------------------------*/
/* check_heap_validity: Debug-only integrity checks for the heap and  */
/* free list. Returns TRUE if all checks pass, FALSE otherwise.       */
/* Checks: heap bounds, every block valid, free list nodes are free,  */
/* no adjacent free blocks remain uncoalesced.                         */
/*--------------------------------------------------------------------*/
#ifndef NDEBUG
static int
check_heap_validity(void)
{
    Chunk_T w;

    if (s_heap_lo == NULL) {
        fprintf(stderr, "Uninitialized heap start\n");
        return FALSE;
    }
    if (s_heap_hi == NULL) {
        fprintf(stderr, "Uninitialized heap end\n");
        return FALSE;
    }
    if (s_heap_lo == s_heap_hi) {
        if (s_free_head == NULL)
            return TRUE;
        fprintf(stderr, "Inconsistent empty heap\n");
        return FALSE;
    }

    /* Walk all physical blocks in address order. */
    for (w = (Chunk_T)s_heap_lo;
         w && w < (Chunk_T)s_heap_hi;
         w = chunk_get_adjacent(
             w, s_heap_lo, s_heap_hi))
    {
        if (!chunk_is_valid(w, s_heap_lo, s_heap_hi))
            return FALSE;
    }

    /* Walk the free list. */
    for (w = s_free_head; w != NULL;
         w = chunk_get_next_free(w))
    {
        Chunk_T adj;
        if (chunk_get_status(w) != CHUNK_FREE) {
            fprintf(stderr,
                    "Non-free chunk in free list\n");
            return FALSE;
        }
        if (!chunk_is_valid(w, s_heap_lo, s_heap_hi))
            return FALSE;

        adj = chunk_get_adjacent(
            w, s_heap_lo, s_heap_hi);
        if (adj != NULL
            && chunk_get_status(adj) == CHUNK_FREE)
        {
            fprintf(stderr,
                    "Uncoalesced adjacent free\n");
            return FALSE;
        }
    }
    return TRUE;
}
#endif /* NDEBUG */

/*--------------------------------------------------------------------*/
/* bytes_to_units: Convert byte count to payload units (rounded up).  */
/* Does not include header or footer overhead.                         */
/*--------------------------------------------------------------------*/
static size_t
bytes_to_units(size_t bytes)
{
    return (bytes + CHUNK_UNIT - 1) / CHUNK_UNIT;
}

/*--------------------------------------------------------------------*/
/* payload_units: Return the number of payload units in a block,      */
/* which is span minus the overhead (header + footer).                 */
/*--------------------------------------------------------------------*/
static size_t
payload_units(Chunk_T c)
{
    return (size_t)(chunk_get_span_units(c)
                    - OVERHEAD_UNITS);
}

/*--------------------------------------------------------------------*/
/* header_from_payload: Map a client pointer back to the block header */
/* by stepping one unit backward.                                      */
/*--------------------------------------------------------------------*/
static Chunk_T
header_from_payload(void *p)
{
    return (Chunk_T)((char *)p - CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* payload_from_header: Map a block header to the client pointer by   */
/* stepping one unit forward.                                          */
/*--------------------------------------------------------------------*/
static void *
payload_from_header(Chunk_T c)
{
    return (void *)((char *)c + CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* heap_bootstrap: Initialize heap bounds via sbrk(0). Must be called */
/* exactly once before the first allocation. Exits on failure.         */
/*--------------------------------------------------------------------*/
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
/* fl_remove: Remove chunk c from the doubly-linked free list in O(1) */
/* using the prev and next pointers.                                   */
/*--------------------------------------------------------------------*/
static void
fl_remove(Chunk_T c)
{
    Chunk_T p = chunk_get_prev_free(c);
    Chunk_T n = chunk_get_next_free(c);

    if (p != NULL)
        chunk_set_next_free(p, n);
    else
        s_free_head = n;

    if (n != NULL)
        chunk_set_prev_free(n, p);
}

/*--------------------------------------------------------------------*/
/* fl_insert_front: Insert chunk c at the head of the free list.      */
/* Sets c's status to CHUNK_FREE and updates all pointers.             */
/*--------------------------------------------------------------------*/
static void
fl_insert_front(Chunk_T c)
{
    chunk_set_status(c, CHUNK_FREE);
    chunk_set_prev_free(c, NULL);
    chunk_set_next_free(c, s_free_head);

    if (s_free_head != NULL)
        chunk_set_prev_free(s_free_head, c);

    s_free_head = c;
}

/*--------------------------------------------------------------------*/
/* split_block: Given a free block c with enough room, split off an   */
/* allocated block of (OVERHEAD_UNITS + need_units) from the tail.    */
/* The leading portion remains free. Returns the new allocated block.  */
/*--------------------------------------------------------------------*/
static Chunk_T
split_block(Chunk_T c, size_t need_units)
{
    Chunk_T alloc;
    int old_span = chunk_get_span_units(c);
    int alloc_span = (int)(OVERHEAD_UNITS + need_units);
    int remain_span = old_span - alloc_span;

    assert(chunk_get_status(c) == CHUNK_FREE);
    assert(remain_span >= MIN_BLOCK_SPAN);

    /* Shrink the leading free block. */
    chunk_set_span_units(c, remain_span);

    /* Create the allocated block right after. */
    alloc = chunk_get_adjacent(
        c, s_heap_lo, s_heap_hi);
    chunk_set_span_units(alloc, alloc_span);
    chunk_set_status(alloc, CHUNK_USED);

    return alloc;
}

/*--------------------------------------------------------------------*/
/* grow_heap: Request more memory from OS. Creates a new block and    */
/* coalesces with the physically previous block if it is free.         */
/* Returns the new free block, or NULL on failure.                     */
/*--------------------------------------------------------------------*/
static Chunk_T
grow_heap(size_t need_units)
{
    Chunk_T c;
    Chunk_T prev_phys;
    size_t grow_data;
    size_t grow_span;

    grow_data = (need_units < MIN_GROW_UNITS)
                ? MIN_GROW_UNITS : need_units;
    grow_span = OVERHEAD_UNITS + grow_data;

    c = (Chunk_T)sbrk((int)(grow_span * CHUNK_UNIT));
    if (c == (Chunk_T)-1)
        return NULL;

    s_heap_hi = sbrk(0);

    chunk_set_span_units(c, (int)grow_span);
    chunk_set_status(c, CHUNK_USED);

    /* Coalesce with previous physical block if free. */
    prev_phys = chunk_get_prev_adjacent(
        c, s_heap_lo);
    if (prev_phys != NULL
        && chunk_get_status(prev_phys) == CHUNK_FREE)
    {
        fl_remove(prev_phys);
        chunk_set_span_units(
            prev_phys,
            chunk_get_span_units(prev_phys)
            + chunk_get_span_units(c));
        c = prev_phys;
    }

    fl_insert_front(c);

    assert(check_heap_validity());
    return c;
}

/*--------------------------------------------------------------------*/
/* find_fit: First-fit scan of the free list. Returns the first free  */
/* block with enough payload for need_units, or NULL if none found.   */
/*--------------------------------------------------------------------*/
static Chunk_T
find_fit(size_t need_units)
{
    Chunk_T cur;
    for (cur = s_free_head; cur != NULL;
         cur = chunk_get_next_free(cur))
    {
        if (payload_units(cur) >= need_units)
            return cur;
    }
    return NULL;
}

/*--------------------------------------------------------------------*/
/* allocate_from: Given a free block c, either split it or use it     */
/* whole. Returns a pointer to the usable payload.                     */
/*--------------------------------------------------------------------*/
static void *
allocate_from(Chunk_T c, size_t need_units)
{
    size_t avail = payload_units(c);

    /* Split if the leftover can form a valid block. */
    if (avail >= need_units + MIN_BLOCK_SPAN) {
        c = split_block(c, need_units);
    }
    else {
        /* Use the entire block as-is. */
        fl_remove(c);
        chunk_set_status(c, CHUNK_USED);
    }
    return payload_from_header(c);
}

/*--------------------------------------------------------------------*/
/* heapmgr_malloc: Allocate at least 'size' bytes. Returns a pointer  */
/* to uninitialized space, or NULL if size is 0 or allocation fails.  */
/* Strategy: first-fit on free list, split if possible, grow heap if  */
/* no suitable block is found.                                         */
/*--------------------------------------------------------------------*/
void *
heapmgr_malloc(size_t size)
{
    static int booted = FALSE;
    Chunk_T cur;
    size_t need_units;

    if (size == 0)
        return NULL;

    if (!booted) {
        heap_bootstrap();
        booted = TRUE;
    }

    assert(check_heap_validity());

    need_units = bytes_to_units(size);

    /* Ensure minimum 1 unit payload for prev_free. */
    if (need_units < 1)
        need_units = 1;

    /* Search the free list for a fit. */
    cur = find_fit(need_units);
    if (cur != NULL) {
        void *result = allocate_from(cur, need_units);
        assert(check_heap_validity());
        return result;
    }

    /* No fit: grow the heap. */
    cur = grow_heap(need_units);
    if (cur == NULL) {
        assert(check_heap_validity());
        return NULL;
    }

    {
        void *result = allocate_from(cur, need_units);
        assert(check_heap_validity());
        return result;
    }
}

/*--------------------------------------------------------------------*/
/* coalesce_neighbors: For block c, check the physically next and     */
/* previous blocks. If either is free, merge them with c. Returns     */
/* the final merged block.                                             */
/*--------------------------------------------------------------------*/
static Chunk_T
coalesce_neighbors(Chunk_T c)
{
    Chunk_T neighbor;

    /* Merge with the NEXT physical neighbor. */
    neighbor = chunk_get_adjacent(
        c, s_heap_lo, s_heap_hi);
    if (neighbor != NULL
        && chunk_get_status(neighbor) == CHUNK_FREE)
    {
        fl_remove(neighbor);
        chunk_set_span_units(
            c,
            chunk_get_span_units(c)
            + chunk_get_span_units(neighbor));
    }

    /* Merge with the PREVIOUS physical neighbor. */
    neighbor = chunk_get_prev_adjacent(
        c, s_heap_lo);
    if (neighbor != NULL
        && chunk_get_status(neighbor) == CHUNK_FREE)
    {
        fl_remove(neighbor);
        chunk_set_span_units(
            neighbor,
            chunk_get_span_units(neighbor)
            + chunk_get_span_units(c));
        c = neighbor;
    }

    return c;
}

/*--------------------------------------------------------------------*/
/* heapmgr_free: Free a previously allocated block. Coalesces with    */
/* adjacent free neighbors using boundary tags for O(1) merging.      */
/* If p is NULL, does nothing.                                         */
/*--------------------------------------------------------------------*/
void
heapmgr_free(void *p)
{
    Chunk_T c;

    if (p == NULL)
        return;

    assert(check_heap_validity());

    c = header_from_payload(p);
    assert(chunk_get_status(c) != CHUNK_FREE);

    c = coalesce_neighbors(c);
    fl_insert_front(c);

    assert(check_heap_validity());
}
