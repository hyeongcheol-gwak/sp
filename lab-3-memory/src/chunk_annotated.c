/*====================================================================*/
/* chunk.c (한글 주석 버전)                                            */
/*                                                                    */
/* Chunk 모듈: 힙 메모리 블록의 자료구조와 조작 함수를 정의한다.        */
/*                                                                    */
/* [블록 메모리 레이아웃]                                               */
/*                                                                    */
/*   각 블록은 아래와 같이 구성된다 (각 슬롯 = CHUNK_UNIT = 16 bytes): */
/*                                                                    */
/*   +--------+------------------+--------+                           */
/*   | Header | Payload 영역 ... | Footer |                           */
/*   +--------+------------------+--------+                           */
/*                                                                    */
/*   Header (struct Chunk, 16 bytes):                                 */
/*     - int status  : 블록 상태 (CHUNK_FREE=0 또는 CHUNK_USED=1)     */
/*     - int span    : 블록 전체 크기 (unit 단위)                      */
/*     - Chunk_T next_free : 다음 free 블록 포인터                     */
/*                                                                    */
/*   Footer (마지막 1 unit의 첫 4 bytes):                             */
/*     - int span : header의 span과 동일한 값을 복사해서 저장          */
/*     → 이전 블록으로 역방향 탐색 시 사용 (boundary tag 기법)          */
/*                                                                    */
/*   Free 블록 전용:                                                   */
/*     - payload 영역의 첫 번째 unit에 prev_free 포인터를 저장         */
/*     → 이중 연결 리스트 구현을 위해 필요                              */
/*                                                                    */
/*   사용 중(USED) 블록:                                               */
/*     - payload 영역 전체가 사용자 데이터로 사용됨                     */
/*     - next_free, prev_free 포인터는 의미 없음                       */
/*                                                                    */
/*   최소 블록 크기: MIN_BLOCK_SPAN = 3 units                         */
/*     → header(1) + payload/prev_free(최소 1) + footer(1)            */
/*====================================================================*/

#include <stdio.h>
#include <stddef.h>
#include <unistd.h>
#include <stdlib.h>
#include <assert.h>

#include "chunk.h"

/*--------------------------------------------------------------------*/
/* struct Chunk: 모든 블록의 헤더 구조체.                               */
/*                                                                    */
/* 크기가 정확히 CHUNK_UNIT(16 bytes)이 되도록 설계되었다:             */
/*   int status   (4 bytes) : FREE/USED 상태                          */
/*   int span     (4 bytes) : 블록 전체 span (unit 단위)              */
/*   Chunk_T next_free (8 bytes) : 다음 free 블록 포인터              */
/*   합계 = 16 bytes = CHUNK_UNIT                                     */
/*                                                                    */
/* 이 구조체 하나가 메모리에서 하나의 "unit"을 차지하며,                */
/* 블록의 맨 앞에 위치한다.                                             */
/*--------------------------------------------------------------------*/
struct Chunk {
    int     status;      /* CHUNK_FREE(0) 또는 CHUNK_USED(1) */
    int     span;        /* 블록 전체 크기 (header + payload + footer, unit 단위) */
    Chunk_T next_free;   /* free list에서 다음 free 블록을 가리키는 포인터 */
};

/*--------------------------------------------------------------------*/
/* get_footer_ptr: 블록 c의 footer 위치를 반환한다.                    */
/*                                                                    */
/* Footer는 블록의 마지막 unit에 위치하며, span 값을 int로 저장한다.    */
/* 계산: 블록 시작 주소 + (span - 1) * CHUNK_UNIT                      */
/*                                                                    */
/* 예) span=5인 블록:                                                  */
/*   [Header][unit1][unit2][unit3][Footer]                             */
/*    ^c                          ^footer (c + 4*16)                   */
/*--------------------------------------------------------------------*/
static int *get_footer_ptr(Chunk_T c)
{
    return (int *)((char *)c
                   + (c->span - 1) * CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* get_prev_free_slot: free 블록에서 prev_free 포인터가 저장된         */
/* 위치를 반환한다.                                                     */
/*                                                                    */
/* prev_free는 payload 영역의 첫 번째 unit에 저장된다.                 */
/* 즉, header 바로 다음 위치 = header 주소 + CHUNK_UNIT.               */
/*                                                                    */
/* 이 포인터는 free 블록에서만 유효하다.                                */
/* 사용 중인 블록에서는 이 위치가 사용자 데이터의 일부이다.              */
/*--------------------------------------------------------------------*/
static Chunk_T *get_prev_free_slot(Chunk_T c)
{
    return (Chunk_T *)((char *)c + CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* chunk_get_status: 블록 c의 상태를 반환한다.                         */
/* 반환값: CHUNK_FREE(0) 또는 CHUNK_USED(1)                            */
/*--------------------------------------------------------------------*/
int chunk_get_status(Chunk_T c)
{
    return c->status;
}

/*--------------------------------------------------------------------*/
/* chunk_set_status: 블록 c의 상태를 설정한다.                         */
/* status: CHUNK_FREE(0) 또는 CHUNK_USED(1)                            */
/*--------------------------------------------------------------------*/
void chunk_set_status(Chunk_T c, int status)
{
    c->status = status;
}

/*--------------------------------------------------------------------*/
/* chunk_get_span_units: 블록 c의 전체 span을 반환한다.                */
/* span = header(1) + payload(n) + footer(1) = n + 2 units            */
/*--------------------------------------------------------------------*/
int chunk_get_span_units(Chunk_T c)
{
    return c->span;
}

/*--------------------------------------------------------------------*/
/* chunk_set_span_units: 블록 c의 span을 header와 footer 양쪽에       */
/* 동시에 설정한다.                                                     */
/*                                                                    */
/* header와 footer에 같은 span 값을 저장하는 것이 boundary tag의       */
/* 핵심이다. footer에 span이 있으므로 다음 블록에서 이전 블록의        */
/* 시작 위치를 역산할 수 있다.                                          */
/*                                                                    */
/* 예) 블록 B의 시작 주소를 알고 싶을 때:                               */
/*   B 바로 앞의 footer에서 span을 읽고,                               */
/*   B_start = footer_addr - (span - 1) * CHUNK_UNIT                  */
/*--------------------------------------------------------------------*/
void chunk_set_span_units(Chunk_T c, int span)
{
    int *footer;
    c->span = span;          /* header에 span 저장 */
    footer = get_footer_ptr(c);
    *footer = span;          /* footer에도 같은 span 저장 (boundary tag) */
}

/*--------------------------------------------------------------------*/
/* chunk_get_next_free: free list에서 다음 free 블록을 반환한다.        */
/* header의 next_free 필드에서 읽어온다.                                */
/* 마지막 블록이면 NULL을 반환한다.                                     */
/*--------------------------------------------------------------------*/
Chunk_T chunk_get_next_free(Chunk_T c)
{
    return c->next_free;
}

/*--------------------------------------------------------------------*/
/* chunk_set_next_free: free list에서 다음 free 블록 포인터를 설정한다. */
/* header의 next_free 필드에 기록한다.                                  */
/*--------------------------------------------------------------------*/
void chunk_set_next_free(Chunk_T c, Chunk_T next)
{
    c->next_free = next;
}

/*--------------------------------------------------------------------*/
/* chunk_get_prev_free: free list에서 이전 free 블록을 반환한다.        */
/*                                                                    */
/* prev_free 포인터는 payload 영역의 첫 unit에 저장되어 있다.          */
/* (header 구조체에는 공간이 없으므로 payload 영역을 빌려쓴다)          */
/*                                                                    */
/* free 블록에서만 호출해야 한다. 사용 중인 블록에서 호출하면            */
/* 사용자 데이터를 포인터로 잘못 해석하게 된다.                          */
/*--------------------------------------------------------------------*/
Chunk_T chunk_get_prev_free(Chunk_T c)
{
    return *get_prev_free_slot(c);
}

/*--------------------------------------------------------------------*/
/* chunk_set_prev_free: free list에서 이전 free 블록 포인터를 설정한다. */
/* payload 영역의 첫 unit에 기록한다.                                   */
/*--------------------------------------------------------------------*/
void chunk_set_prev_free(Chunk_T c, Chunk_T prev)
{
    *get_prev_free_slot(c) = prev;
}

/*--------------------------------------------------------------------*/
/* chunk_get_adjacent: 물리적으로 바로 다음에 위치한 블록을 반환한다.    */
/*                                                                    */
/* 현재 블록의 시작 주소에서 span만큼 앞으로 이동하면 다음 블록이다.     */
/*   다음 블록 주소 = c + c->span  (포인터 산술, 단위: struct Chunk)    */
/*                                                                    */
/* 다음 블록이 힙 영역(end)을 벗어나면 NULL을 반환한다.                 */
/* 즉, c가 힙의 마지막 블록인 경우 NULL.                                */
/*--------------------------------------------------------------------*/
Chunk_T chunk_get_adjacent(Chunk_T c, void *start, void *end)
{
    Chunk_T n;
    assert((void *)c >= start);  /* c가 힙 시작 이후인지 확인 */
    n = c + c->span;             /* span units만큼 앞으로 이동 */
    if ((void *)n >= end)        /* 힙 끝을 벗어나면 */
        return NULL;             /* 다음 블록 없음 */
    return n;
}

/*--------------------------------------------------------------------*/
/* chunk_get_prev_adjacent: 물리적으로 바로 이전에 위치한 블록을         */
/* 반환한다.                                                            */
/*                                                                    */
/* ★ Boundary Tag의 핵심 활용 ★                                       */
/*                                                                    */
/* 현재 블록 c 바로 앞의 unit은 이전 블록의 footer이다.                 */
/* footer에 저장된 span 값을 읽으면 이전 블록의 크기를 알 수 있고,      */
/* 거기서 역산하면 이전 블록의 시작 위치를 구할 수 있다.                 */
/*                                                                    */
/*   이전 블록 시작 = (char*)c - prev_span * CHUNK_UNIT               */
/*                                                                    */
/* 만약 c가 힙의 첫 블록(c == start)이면 이전 블록이 없으므로           */
/* NULL을 반환한다.                                                     */
/*                                                                    */
/* 이 함수 덕분에 free() 시 이전 블록과의 합병(coalesce)이              */
/* O(1)에 가능하다. (boundary tag가 없으면 처음부터 순회해야 하므로      */
/* O(n)이 된다)                                                        */
/*--------------------------------------------------------------------*/
Chunk_T chunk_get_prev_adjacent(Chunk_T c, void *start)
{
    int prev_span;
    if ((void *)c <= start)      /* c가 힙의 첫 블록이면 */
        return NULL;             /* 이전 블록 없음 */
    /* c 바로 앞의 unit(= 이전 블록의 footer)에서 span을 읽는다 */
    prev_span = *((int *)((char *)c - CHUNK_UNIT));
    /* span만큼 뒤로 이동하면 이전 블록의 header */
    return (Chunk_T)((char *)c - prev_span * CHUNK_UNIT);
}

#ifndef NDEBUG
/*--------------------------------------------------------------------*/
/* chunk_is_valid: 디버그 모드에서 블록의 유효성을 검증한다.             */
/*                                                                    */
/* 검증 항목:                                                          */
/*   1) 블록이 힙 시작 주소 이후인가? (c >= start)                     */
/*   2) 블록이 힙 끝 주소 이전인가? (c < end)                          */
/*   3) span이 최소 블록 크기(MIN_BLOCK_SPAN=3) 이상인가?              */
/*                                                                    */
/* 유효하면 1, 아니면 0을 반환하고 stderr에 에러 메시지를 출력한다.      */
/*                                                                    */
/* NDEBUG 매크로가 정의되면 (릴리즈 빌드) 이 함수는 컴파일되지 않는다.  */
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
