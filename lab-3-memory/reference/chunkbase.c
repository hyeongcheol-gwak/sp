/*--------------------------------------------------------------------*/
/* chunkbase.c                                                        */
/*--------------------------------------------------------------------*/

#include <stdio.h>
#include <stddef.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>

#include "chunkbase.h"

/* Internal header layout
 * - status: CHUNK_FREE or CHUNK_USED
 * - span:   total units, including the header itself
 * - next:   next-free pointer for the singly-linked free list
 */
struct Chunk {
    int     status;
    int     span;
    Chunk_T next;
};

/* ----------------------- Getters / Setters ------------------------ */
int   chunk_get_status(Chunk_T c)                 { return c->status; }
void  chunk_set_status(Chunk_T c, int status)     { c->status = status; }

int   chunk_get_span_units(Chunk_T c)             { return c->span; }
void  chunk_set_span_units(Chunk_T c, int span_u) { c->span = span_u; }

Chunk_T chunk_get_next_free(Chunk_T c)            { return c->next; }
void    chunk_set_next_free(Chunk_T c, Chunk_T n) { c->next = n; }

/* chunk_get_adjacent:
 * Compute the next physical block header by jumping 'span' units
 * (1 header + payload) forward from 'c'. If that address falls at or
 * beyond 'end', return NULL to indicate there is no next block. */
Chunk_T
chunk_get_adjacent(Chunk_T c, void *start, void *end)
{
    Chunk_T n;

    assert((void *)c >= start);

    n = c + c->span;                /* span includes the header itself */

    if ((void *)n >= end)
        return NULL;

    return n;
}

#ifndef NDEBUG
/* chunk_is_valid:
 * Minimal per-block validity checks used by the heap validator:
 *  - c must lie within [start, end)
 *  - span must be positive (non-zero) */
int
chunk_is_valid(Chunk_T c, void *start, void *end)
{
    assert(c     != NULL);
    assert(start != NULL);
    assert(end   != NULL);

    if (c < (Chunk_T)start) { fprintf(stderr, "Bad heap start\n"); return 0; }
    if (c >= (Chunk_T)end)  { fprintf(stderr, "Bad heap end\n");   return 0; }
    if (c->span <= 0)       { fprintf(stderr, "Non-positive span\n"); return 0; }
    return 1;
}
#endif
