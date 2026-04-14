/*--------------------------------------------------------------------*/
/* chunkbase.h                                                        */
/*--------------------------------------------------------------------*/

#ifndef _CHUNK_BASE_H_
#define _CHUNK_BASE_H_

#pragma once

#include <stdbool.h>
#include <unistd.h>

/*
   Representation used in this baseline:
   - Each *allocated block* consists of one header Chunk (1 unit) and
     zero or more payload Chunks (N units).
   - The header stores the *total* number of units (header + payload),
     called "span". Therefore:
         span = 1 (header) + payload_units
   - The free list is a singly-linked list of free blocks ordered by
     increasing address (non-circular).
*/

typedef struct Chunk *Chunk_T;

/* Status flags */
enum {
    CHUNK_FREE = 0,
    CHUNK_USED = 1,
};

/* Chunk unit size (bytes). This equals sizeof(struct Chunk) in this baseline. */
enum {
    CHUNK_UNIT = 16,
};

/* ----------------------- Getters / Setters ------------------------ */

/* chunk_get_status / chunk_set_status:
 * Get/Set the status of a block (CHUNK_FREE or CHUNK_USED). */
int   chunk_get_status(Chunk_T c);
void  chunk_set_status(Chunk_T c, int status);

/* chunk_get_span_units / chunk_set_span_units:
 * Get/Set the total number of units of the block, including its header.
 * (span = 1 header unit + payload units) */
int   chunk_get_span_units(Chunk_T c);
void  chunk_set_span_units(Chunk_T c, int span_units);

/* chunk_get_next_free / chunk_set_next_free:
 * Get/Set the next pointer in the singly-linked free list. */
Chunk_T chunk_get_next_free(Chunk_T c);
void    chunk_set_next_free(Chunk_T c, Chunk_T next);

/* chunk_get_adjacent:
 * Return the physically next adjacent block's header (if any) by walking
 * 'span' units forward from 'c'. Returns NULL if 'c' is the last block.
 * 'start' and 'end' are the inclusive start and exclusive end addresses
 * of the heap region. */
Chunk_T chunk_get_adjacent(Chunk_T c, void *start, void *end);

/* Debug-only sanity check (compiled only if NDEBUG is not defined). */
#ifndef NDEBUG

/* chunk_is_valid:
 * Return 1 iff 'c' lies within [start, end) and has a positive span. */
int   chunk_is_valid(Chunk_T c, void *start, void *end);

#endif

#endif /* _CHUNK_BASE_H_ */
