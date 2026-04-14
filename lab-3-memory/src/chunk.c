/*--------------------------------------------------------------------*/
/* chunk.c                                                            */
/* Implementation of the Chunk module with boundary tags.             */
/*                                                                    */
/* Block layout (each slot = CHUNK_UNIT = 16 bytes):                  */
/*   [Header] [Payload...] [Footer]                                   */
/*                                                                    */
/* Header (struct Chunk):                                             */
/*   int status, int span, Chunk_T next_free                          */
/*                                                                    */
/* Footer: last unit of the block, stores span as an int.             */
/*                                                                    */
/* For free blocks, prev_free pointer is stored at the start of       */
/* the payload area (i.e., one unit after the header).                 */
/*--------------------------------------------------------------------*/

#include <stdio.h>
#include <stddef.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>

#include "chunk.h"

/*--------------------------------------------------------------------*/
/* struct Chunk: the header for every block. Exactly CHUNK_UNIT bytes. */
/*--------------------------------------------------------------------*/
struct Chunk {
    int     status;
    int     span;
    Chunk_T next_free;
};

/*--------------------------------------------------------------------*/
/* get_footer_ptr: Return a pointer to the int at the end of the      */
/* block that stores the span (the footer).                           */
/*--------------------------------------------------------------------*/
static int *get_footer_ptr(Chunk_T c)
{
    return (int *)((char *)c
                   + (c->span - 1) * CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* get_prev_free_slot: Return a pointer to the Chunk_T stored in      */
/* the first unit of the payload area (used for prev_free in free      */
/* blocks).                                                            */
/*--------------------------------------------------------------------*/
static Chunk_T *get_prev_free_slot(Chunk_T c)
{
    return (Chunk_T *)((char *)c + CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* chunk_get_status: Return the status of chunk c.                    */
/*--------------------------------------------------------------------*/
int chunk_get_status(Chunk_T c)
{
    return c->status;
}

/*--------------------------------------------------------------------*/
/* chunk_set_status: Set the status of chunk c.                       */
/*--------------------------------------------------------------------*/
void chunk_set_status(Chunk_T c, int status)
{
    c->status = status;
}

/*--------------------------------------------------------------------*/
/* chunk_get_span_units: Return the total span of chunk c.            */
/*--------------------------------------------------------------------*/
int chunk_get_span_units(Chunk_T c)
{
    return c->span;
}

/*--------------------------------------------------------------------*/
/* chunk_set_span_units: Set the span in both header and footer.      */
/*--------------------------------------------------------------------*/
void chunk_set_span_units(Chunk_T c, int span)
{
    int *footer;
    c->span = span;
    footer = get_footer_ptr(c);
    *footer = span;
}

/*--------------------------------------------------------------------*/
/* chunk_get_next_free: Return the next free chunk in the list.       */
/*--------------------------------------------------------------------*/
Chunk_T chunk_get_next_free(Chunk_T c)
{
    return c->next_free;
}

/*--------------------------------------------------------------------*/
/* chunk_set_next_free: Set the next-free pointer of chunk c.         */
/*--------------------------------------------------------------------*/
void chunk_set_next_free(Chunk_T c, Chunk_T next)
{
    c->next_free = next;
}

/*--------------------------------------------------------------------*/
/* chunk_get_prev_free: Read the prev-free pointer from the payload   */
/* area of a free block.                                               */
/*--------------------------------------------------------------------*/
Chunk_T chunk_get_prev_free(Chunk_T c)
{
    return *get_prev_free_slot(c);
}

/*--------------------------------------------------------------------*/
/* chunk_set_prev_free: Write the prev-free pointer into the payload  */
/* area of a free block.                                               */
/*--------------------------------------------------------------------*/
void chunk_set_prev_free(Chunk_T c, Chunk_T prev)
{
    *get_prev_free_slot(c) = prev;
}

/*--------------------------------------------------------------------*/
/* chunk_get_adjacent: Return the next physical block by jumping      */
/* span units forward. Returns NULL if beyond heap end.                */
/*--------------------------------------------------------------------*/
Chunk_T chunk_get_adjacent(Chunk_T c, void *start, void *end)
{
    Chunk_T n;
    assert((void *)c >= start);
    n = c + c->span;
    if ((void *)n >= end)
        return NULL;
    return n;
}

/*--------------------------------------------------------------------*/
/* chunk_get_prev_adjacent: Return the previous physical block by     */
/* reading the span stored in the footer just before c.                */
/* Returns NULL if c is the first block (c == start).                  */
/*--------------------------------------------------------------------*/
Chunk_T chunk_get_prev_adjacent(Chunk_T c, void *start)
{
    int prev_span;
    if ((void *)c <= start)
        return NULL;
    /* The footer of the previous block is the unit just before c. */
    prev_span = *((int *)((char *)c - CHUNK_UNIT));
    return (Chunk_T)((char *)c - prev_span * CHUNK_UNIT);
}

#ifndef NDEBUG
/*--------------------------------------------------------------------*/
/* chunk_is_valid: Return 1 if c is within [start, end) and has a     */
/* valid positive span >= MIN_BLOCK_SPAN. Returns 0 on failure.       */
/*--------------------------------------------------------------------*/
int chunk_is_valid(Chunk_T c, void *start, void *end)
{
    assert(c != NULL);
    assert(start != NULL);
    assert(end != NULL);

    if (c < (Chunk_T)start) {
        fprintf(stderr, "Bad heap start\n");
        return 0;
    }
    if (c >= (Chunk_T)end) {
        fprintf(stderr, "Bad heap end\n");
        return 0;
    }
    if (c->span < MIN_BLOCK_SPAN) {
        fprintf(stderr, "Span too small: %d\n",
                c->span);
        return 0;
    }
    return 1;
}
#endif