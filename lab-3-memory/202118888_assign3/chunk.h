/*--------------------------------------------------------------------*/
/* chunk.h                                                            */
/* Chunk module for doubly-linked free list with boundary tags.       */
/*                                                                    */
/* Memory layout of one block (in CHUNK_UNIT-sized slots):            */
/*   [Header] [Payload / FreeInfo] ... [Footer]                       */
/*                                                                    */
/* Header   (1 unit): status, span, next_free_ptr                     */
/* Footer   (1 unit): span (for backward coalescing)                  */
/* FreeInfo: prev_free stored at start of payload area (free only)    */
/*                                                                    */
/* Minimum block span = 3 units (header + 1 payload/prev + footer).   */
/* For allocated blocks the payload begins right after the header.    */
/*--------------------------------------------------------------------*/

#ifndef CHUNK_H
#define CHUNK_H

#include <stddef.h>
#include <unistd.h>

/*--------------------------------------------------------------------*/
/* Chunk_T: opaque pointer to a chunk header.                         */
/*--------------------------------------------------------------------*/
typedef struct Chunk *Chunk_T;

/*--------------------------------------------------------------------*/
/* Status flags for a chunk.                                          */
/*--------------------------------------------------------------------*/
enum {
    CHUNK_FREE = 0,
    CHUNK_USED = 1
};

/*--------------------------------------------------------------------*/
/* CHUNK_UNIT: size in bytes of one unit (== sizeof(struct Chunk)).    */
/*--------------------------------------------------------------------*/
enum {
    CHUNK_UNIT = 16
};

/*--------------------------------------------------------------------*/
/* MIN_BLOCK_SPAN: minimum span for any block (header+body+footer).   */
/*--------------------------------------------------------------------*/
enum {
    MIN_BLOCK_SPAN = 3
};

/* ------------- Basic getters / setters ----------------------------- */

/* chunk_get_status: Return CHUNK_FREE or CHUNK_USED for chunk c. */
int chunk_get_status(Chunk_T c);

/* chunk_set_status: Set the status of chunk c. */
void chunk_set_status(Chunk_T c, int status);

/* chunk_get_span_units: Return total span of chunk c (in units). */
int chunk_get_span_units(Chunk_T c);

/* chunk_set_span_units: Set the span of c in both header & footer. */
void chunk_set_span_units(Chunk_T c, int span);

/* ------------- Free-list pointer accessors ------------------------- */

/* chunk_get_next_free: Return the next free chunk, or NULL. */
Chunk_T chunk_get_next_free(Chunk_T c);

/* chunk_set_next_free: Set the next-free pointer of chunk c. */
void chunk_set_next_free(Chunk_T c, Chunk_T next);

/* chunk_get_prev_free: Return the prev free chunk, or NULL. */
Chunk_T chunk_get_prev_free(Chunk_T c);

/* chunk_set_prev_free: Set the prev-free pointer of chunk c. */
void chunk_set_prev_free(Chunk_T c, Chunk_T prev);

/* ------------- Physical neighbor accessors ------------------------- */

/* chunk_get_adjacent: Return physically next block, or NULL. */
Chunk_T chunk_get_adjacent(Chunk_T c, void *start, void *end);

/* chunk_get_prev_adjacent: Return physically previous block using
 * footer of predecessor, or NULL if c is the first block. */
Chunk_T chunk_get_prev_adjacent(Chunk_T c, void *start);

/* ------------- Debug validation ------------------------------------ */

#ifndef NDEBUG
/* chunk_is_valid: Return 1 if c is in [start,end) with valid span. */
int chunk_is_valid(Chunk_T c, void *start, void *end);
#endif

#endif /* CHUNK_H */