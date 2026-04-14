/*--------------------------------------------------------------------*/
/* heapmgr2.c                                                         */
/* Heap manager using segregated-fit bins with boundary tags.         */
/*                                                                    */
/* Key improvements over heapmgr1:                                     */
/* - Multiple size-class bins for fast allocation                      */
/* - Bitmap for O(1) non-empty bin lookup                              */
/* - Doubly-linked free list per bin                                   */
/* - Boundary tags (header + footer) for O(1) coalescing               */
/* - Eliminates worst-case O(n) scan of a single free list             */
/*--------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "chunk.h"

/*--------------------------------------------------------------------*/
/* Boolean constants.                                                  */
/*--------------------------------------------------------------------*/
#define FALSE 0
#define TRUE  1

/*--------------------------------------------------------------------*/
/* OVERHEAD_UNITS: header + footer units per block.                    */
/*--------------------------------------------------------------------*/
enum { OVERHEAD_UNITS = 2 };

/*--------------------------------------------------------------------*/
/* MIN_GROW_UNITS: minimum payload units to request from sbrk.        */
/*--------------------------------------------------------------------*/
enum { MIN_GROW_UNITS = 1024 };

/*--------------------------------------------------------------------*/
/* NUM_BINS: number of segregated free-list bins.                      */
/* Bin layout (by payload units):                                      */
/*   0: [1]    1: [2]   2: [3]   3: [4]                               */
/*   4: [5-8]  5: [9-16] 6: [17-32] 7: [33-64]                       */
/*   8: [65-128] 9: [129-256] 10: [257-512]                           */
/*   11: [513-1024] 12: [1025-2048] 13: [2049-4096]                   */
/*   14: [4097-inf]                                                    */
/*--------------------------------------------------------------------*/
enum { NUM_BINS = 15 };

/*--------------------------------------------------------------------*/
/* Module state: bin heads, bitmap, and heap bounds.                   */
/*--------------------------------------------------------------------*/
static Chunk_T s_bins[NUM_BINS];
static unsigned int s_bitmap = 0;
static void *s_heap_lo = NULL;
static void *s_heap_hi = NULL;

/*--------------------------------------------------------------------*/
/* get_bin_index: Map payload unit count to bin index.                 */
/* Exact bins for sizes 1-4, then power-of-2 ranges.                  */
/*--------------------------------------------------------------------*/
static int
get_bin_index(size_t pu)
{
    if (pu <= 4)
        return (int)(pu - 1);
    if (pu <= 8) return 4;
    if (pu <= 16) return 5;
    if (pu <= 32) return 6;
    if (pu <= 64) return 7;
    if (pu <= 128) return 8;
    if (pu <= 256) return 9;
    if (pu <= 512) return 10;
    if (pu <= 1024) return 11;
    if (pu <= 2048) return 12;
    if (pu <= 4096) return 13;
    return 14;
}

/*--------------------------------------------------------------------*/
/* payload_units: Return payload units of block c.                    */
/*--------------------------------------------------------------------*/
static size_t
payload_units(Chunk_T c)
{
    return (size_t)(chunk_get_span_units(c)
                    - OVERHEAD_UNITS);
}

/*--------------------------------------------------------------------*/
/* check_heap_validity: Debug-only structural integrity check.        */
/* Walks physical blocks and all bins.                                 */
/* Returns TRUE if valid, FALSE otherwise.                             */
/*--------------------------------------------------------------------*/
#ifndef NDEBUG
static int
check_heap_validity(void)
{
    Chunk_T w;
    int i;

    if (s_heap_lo == NULL) {
        fprintf(stderr, "Uninit heap start\n");
        return FALSE;
    }
    if (s_heap_hi == NULL) {
        fprintf(stderr, "Uninit heap end\n");
        return FALSE;
    }
    if (s_heap_lo == s_heap_hi)
        return TRUE;

    for (w = (Chunk_T)s_heap_lo;
         w && w < (Chunk_T)s_heap_hi;
         w = chunk_get_adjacent(
             w, s_heap_lo, s_heap_hi))
    {
        if (!chunk_is_valid(
                w, s_heap_lo, s_heap_hi))
            return FALSE;
    }

    for (i = 0; i < NUM_BINS; i++) {
        for (w = s_bins[i]; w != NULL;
             w = chunk_get_next_free(w))
        {
            Chunk_T adj;
            if (chunk_get_status(w) != CHUNK_FREE) {
                fprintf(stderr,
                    "Non-free in bin %d\n", i);
                return FALSE;
            }
            if (!chunk_is_valid(
                    w, s_heap_lo, s_heap_hi))
                return FALSE;
            adj = chunk_get_adjacent(
                w, s_heap_lo, s_heap_hi);
            if (adj != NULL
                && chunk_get_status(adj)
                   == CHUNK_FREE)
            {
                fprintf(stderr,
                    "Uncoalesced adjacent\n");
                return FALSE;
            }
        }
    }
    return TRUE;
}
#endif

/*--------------------------------------------------------------------*/
/* bytes_to_units: Convert byte count to payload units.               */
/*--------------------------------------------------------------------*/
static size_t
bytes_to_units(size_t bytes)
{
    return (bytes + CHUNK_UNIT - 1) / CHUNK_UNIT;
}

/*--------------------------------------------------------------------*/
/* header_from_payload: Map client pointer to block header.           */
/*--------------------------------------------------------------------*/
static Chunk_T
header_from_payload(void *p)
{
    return (Chunk_T)((char *)p - CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* payload_from_header: Map block header to client pointer.           */
/*--------------------------------------------------------------------*/
static void *
payload_from_header(Chunk_T c)
{
    return (void *)((char *)c + CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* heap_bootstrap: Initialize heap bounds and bins. Called once.       */
/*--------------------------------------------------------------------*/
static void
heap_bootstrap(void)
{
    int i;
    s_heap_lo = s_heap_hi = sbrk(0);
    if (s_heap_lo == (void *)-1) {
        fprintf(stderr, "sbrk(0) failed\n");
        exit(-1);
    }
    for (i = 0; i < NUM_BINS; i++)
        s_bins[i] = NULL;
    s_bitmap = 0;
}

/*--------------------------------------------------------------------*/
/* bin_remove: Remove chunk c from its bin in O(1). Clears the        */
/* bitmap bit if the bin becomes empty.                                 */
/*--------------------------------------------------------------------*/
static void
bin_remove(Chunk_T c)
{
    Chunk_T p = chunk_get_prev_free(c);
    Chunk_T n = chunk_get_next_free(c);
    int idx = get_bin_index(payload_units(c));

    if (p != NULL)
        chunk_set_next_free(p, n);
    else
        s_bins[idx] = n;

    if (n != NULL)
        chunk_set_prev_free(n, p);

    if (s_bins[idx] == NULL)
        s_bitmap &= ~(1u << idx);
}

/*--------------------------------------------------------------------*/
/* bin_insert: Insert chunk c at the head of its bin. Sets the        */
/* bitmap bit and marks c as CHUNK_FREE.                               */
/*--------------------------------------------------------------------*/
static void
bin_insert(Chunk_T c)
{
    int idx = get_bin_index(payload_units(c));

    chunk_set_status(c, CHUNK_FREE);
    chunk_set_prev_free(c, NULL);
    chunk_set_next_free(c, s_bins[idx]);

    if (s_bins[idx] != NULL)
        chunk_set_prev_free(s_bins[idx], c);

    s_bins[idx] = c;
    s_bitmap |= (1u << idx);
}

/*--------------------------------------------------------------------*/
/* split_block: Split free block c, allocating need_units from tail.  */
/* Optimizes by skipping re-binning if the bin index is unchanged.     */
/* Returns the new allocated block.                                    */
/*--------------------------------------------------------------------*/
static Chunk_T
split_block(Chunk_T c, size_t need_units)
{
    Chunk_T alloc;
    int old_span = chunk_get_span_units(c);
    int alloc_span = (int)(OVERHEAD_UNITS + need_units);
    int remain_span = old_span - alloc_span;
    int old_bin, new_bin;

    assert(chunk_get_status(c) == CHUNK_FREE);
    assert(remain_span >= MIN_BLOCK_SPAN);

    old_bin = get_bin_index(
        (size_t)(old_span - OVERHEAD_UNITS));
    new_bin = get_bin_index(
        (size_t)(remain_span - OVERHEAD_UNITS));

    if (old_bin == new_bin) {
        chunk_set_span_units(c, remain_span);
    }
    else {
        bin_remove(c);
        chunk_set_span_units(c, remain_span);
        bin_insert(c);
    }

    alloc = chunk_get_adjacent(
        c, s_heap_lo, s_heap_hi);
    chunk_set_span_units(alloc, alloc_span);
    chunk_set_status(alloc, CHUNK_USED);

    return alloc;
}

/*--------------------------------------------------------------------*/
/* grow_heap: Request more memory via sbrk. Coalesces with previous   */
/* physical block if free. Returns the new free block or NULL.         */
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

    prev_phys = chunk_get_prev_adjacent(
        c, s_heap_lo);
    if (prev_phys != NULL
        && chunk_get_status(prev_phys) == CHUNK_FREE)
    {
        bin_remove(prev_phys);
        chunk_set_span_units(
            prev_phys,
            chunk_get_span_units(prev_phys)
            + chunk_get_span_units(c));
        c = prev_phys;
    }

    bin_insert(c);

    assert(check_heap_validity());
    return c;
}

/*--------------------------------------------------------------------*/
/* find_fit: Use the bitmap to skip empty bins and find a suitable    */
/* free block in O(1) amortized time. Returns first fit or NULL.      */
/*--------------------------------------------------------------------*/
static Chunk_T
find_fit(size_t need_units)
{
    int idx = get_bin_index(need_units);
    unsigned int mask;
    Chunk_T cur;

    /* Check the target bin for a fit. */
    for (cur = s_bins[idx]; cur != NULL;
         cur = chunk_get_next_free(cur))
    {
        if (payload_units(cur) >= need_units)
            return cur;
    }

    /* Use bitmap to jump to next non-empty bin. */
    mask = s_bitmap & ~((1u << (idx + 1)) - 1);
    if (mask == 0)
        return NULL;

    /* Find lowest set bit = smallest bin with blocks. */
    idx = 0;
    while ((mask & 1u) == 0) {
        mask >>= 1;
        idx++;
    }

    return s_bins[idx];
}

/*--------------------------------------------------------------------*/
/* allocate_from: Split or use a free block for the allocation.       */
/* Returns the client payload pointer.                                 */
/*--------------------------------------------------------------------*/
static void *
allocate_from(Chunk_T c, size_t need_units)
{
    size_t avail = payload_units(c);

    if (avail >= need_units + MIN_BLOCK_SPAN) {
        c = split_block(c, need_units);
    }
    else {
        bin_remove(c);
        chunk_set_status(c, CHUNK_USED);
    }
    return payload_from_header(c);
}

/*--------------------------------------------------------------------*/
/* heapmgr_malloc: Allocate at least 'size' bytes. Returns pointer    */
/* to uninitialized space or NULL on failure/zero size.                 */
/* Uses segregated bins with bitmap for fast lookup.                    */
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
    if (need_units < 1)
        need_units = 1;

    cur = find_fit(need_units);
    if (cur != NULL) {
        void *result =
            allocate_from(cur, need_units);
        assert(check_heap_validity());
        return result;
    }

    cur = grow_heap(need_units);
    if (cur == NULL) {
        assert(check_heap_validity());
        return NULL;
    }

    {
        void *result =
            allocate_from(cur, need_units);
        assert(check_heap_validity());
        return result;
    }
}

/*--------------------------------------------------------------------*/
/* coalesce_neighbors: Merge c with adjacent free blocks. Removes     */
/* merged neighbors from their bins. Returns the merged block.         */
/*--------------------------------------------------------------------*/
static Chunk_T
coalesce_neighbors(Chunk_T c)
{
    Chunk_T neighbor;

    neighbor = chunk_get_adjacent(
        c, s_heap_lo, s_heap_hi);
    if (neighbor != NULL
        && chunk_get_status(neighbor) == CHUNK_FREE)
    {
        bin_remove(neighbor);
        chunk_set_span_units(
            c,
            chunk_get_span_units(c)
            + chunk_get_span_units(neighbor));
    }

    neighbor = chunk_get_prev_adjacent(
        c, s_heap_lo);
    if (neighbor != NULL
        && chunk_get_status(neighbor) == CHUNK_FREE)
    {
        bin_remove(neighbor);
        chunk_set_span_units(
            neighbor,
            chunk_get_span_units(neighbor)
            + chunk_get_span_units(c));
        c = neighbor;
    }

    return c;
}

/*--------------------------------------------------------------------*/
/* heapmgr_free: Free a block. Coalesces with adjacent free blocks    */
/* and inserts into the correct bin. Does nothing if p is NULL.         */
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
    bin_insert(c);

    assert(check_heap_validity());
}
