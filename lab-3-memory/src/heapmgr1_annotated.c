/*====================================================================*/
/* heapmgr1.c (한글 주석 버전)                                         */
/*                                                                    */
/* 단일 이중 연결 free list + boundary tag를 사용하는 힙 매니저.        */
/*                                                                    */
/* 설계 요약:                                                          */
/*   - 블록 구조: Header(1 unit) + Payload + Footer(1 unit)           */
/*   - 오버헤드: 블록당 2 units (= 32 bytes)                          */
/*   - Free list: 이중 연결, 비순환, LIFO 삽입                         */
/*   - 할당 전략: First-fit (처음 맞는 블록 사용)                       */
/*   - 합병: Boundary tag로 양방향 O(1) 합병                          */
/*   - 최소 블록: 3 units (header + payload 1 + footer)               */
/*====================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

#include "chunk.h"

#define FALSE 0
#define TRUE  1

/* 블록당 오버헤드: header(1) + footer(1) = 2 units */
enum { OVERHEAD_UNITS = 2 };

/* sbrk로 최소 요청하는 payload 크기 (작은 요청이 반복될 때 sbrk 호출 횟수를 줄임) */
enum { MIN_GROW_UNITS = 1024 };

/*--------------------------------------------------------------------*/
/* 모듈 전역 상태                                                      */
/*   s_free_head: free list의 첫 번째 노드 (NULL이면 빈 리스트)        */
/*   s_heap_lo: 힙 시작 주소 (첫 블록의 header 위치)                   */
/*   s_heap_hi: 힙 끝 주소 (마지막 블록 다음 주소)                     */
/*--------------------------------------------------------------------*/
static Chunk_T s_free_head = NULL;
static void *s_heap_lo = NULL;
static void *s_heap_hi = NULL;

/*--------------------------------------------------------------------*/
/* check_heap_validity: 디버그 모드 전용 힙 무결성 검사.                */
/*                                                                    */
/* 검사 항목:                                                          */
/*   1) 힙 범위 초기화 여부                                            */
/*   2) 모든 물리적 블록의 유효성 (주소, span 등)                       */
/*   3) free list의 모든 노드가 실제로 FREE 상태인지                    */
/*   4) 인접한 free 블록이 합병되지 않고 남아있지는 않은지               */
/*      (합병 누락 = 버그)                                              */
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
    /* 힙이 비어있으면 free list도 비어있어야 함 */
    if (s_heap_lo == s_heap_hi) {
        if (s_free_head == NULL)
            return TRUE;
        fprintf(stderr, "Inconsistent empty heap\n");
        return FALSE;
    }

    /* 모든 물리적 블록을 주소 순서대로 순회하며 유효성 검사 */
    for (w = (Chunk_T)s_heap_lo;
         w && w < (Chunk_T)s_heap_hi;
         w = chunk_get_adjacent(w, s_heap_lo, s_heap_hi))
    {
        if (!chunk_is_valid(w, s_heap_lo, s_heap_hi))
            return FALSE;
    }

    /* free list를 순회하며 검사 */
    for (w = s_free_head; w != NULL;
         w = chunk_get_next_free(w))
    {
        Chunk_T adj;
        /* free list에 USED 블록이 들어있으면 버그 */
        if (chunk_get_status(w) != CHUNK_FREE) {
            fprintf(stderr, "Non-free chunk in free list\n");
            return FALSE;
        }
        if (!chunk_is_valid(w, s_heap_lo, s_heap_hi))
            return FALSE;

        /* 다음 물리적 블록도 free이면 합병이 안 된 것 = 버그 */
        adj = chunk_get_adjacent(w, s_heap_lo, s_heap_hi);
        if (adj != NULL
            && chunk_get_status(adj) == CHUNK_FREE)
        {
            fprintf(stderr, "Uncoalesced adjacent free\n");
            return FALSE;
        }
    }
    return TRUE;
}
#endif

/*--------------------------------------------------------------------*/
/* bytes_to_units: 바이트 수를 payload unit 수로 변환 (올림).           */
/* 예) 1~16 bytes → 1 unit, 17~32 bytes → 2 units                    */
/* 오버헤드(header/footer)는 포함하지 않는다.                           */
/*--------------------------------------------------------------------*/
static size_t
bytes_to_units(size_t bytes)
{
    return (bytes + CHUNK_UNIT - 1) / CHUNK_UNIT;
}

/*--------------------------------------------------------------------*/
/* payload_units: 블록 c의 payload unit 수를 반환한다.                  */
/* payload = 전체 span - 오버헤드(header+footer=2)                     */
/*--------------------------------------------------------------------*/
static size_t
payload_units(Chunk_T c)
{
    return (size_t)(chunk_get_span_units(c) - OVERHEAD_UNITS);
}

/*--------------------------------------------------------------------*/
/* header_from_payload: 사용자 포인터(payload 시작)에서 header를 역산.  */
/* header는 payload 바로 앞 1 unit에 위치한다.                          */
/*   header = (char*)payload - CHUNK_UNIT                              */
/*--------------------------------------------------------------------*/
static Chunk_T
header_from_payload(void *p)
{
    return (Chunk_T)((char *)p - CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* payload_from_header: header에서 사용자 포인터(payload 시작)를 계산.  */
/*   payload = (char*)header + CHUNK_UNIT                              */
/*--------------------------------------------------------------------*/
static void *
payload_from_header(Chunk_T c)
{
    return (void *)((char *)c + CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* heap_bootstrap: 힙 초기화. sbrk(0)으로 현재 힙 끝 주소를 얻는다.    */
/* 최초 한 번만 호출되며, s_heap_lo와 s_heap_hi를 같은 값으로 설정.     */
/* (아직 블록이 없는 빈 힙 상태)                                        */
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
/* fl_remove: free list에서 블록 c를 O(1)에 제거한다.                   */
/*                                                                    */
/* 이중 연결 리스트이므로 prev와 next를 직접 연결하면 된다.             */
/*   - prev가 있으면: prev->next = c->next                             */
/*   - prev가 없으면: c가 head → s_free_head = c->next                */
/*   - next가 있으면: next->prev = c->prev                             */
/*                                                                    */
/* ★ 단일 연결 리스트에서는 prev를 찾기 위해 처음부터 순회해야 하므로   */
/*    O(n)이지만, 이중 연결이면 O(1). 이것이 heapmgr1의 핵심 개선점.    */
/*--------------------------------------------------------------------*/
static void
fl_remove(Chunk_T c)
{
    Chunk_T p = chunk_get_prev_free(c);
    Chunk_T n = chunk_get_next_free(c);

    if (p != NULL)
        chunk_set_next_free(p, n);   /* prev의 next를 c의 next로 */
    else
        s_free_head = n;             /* c가 head였으면 head 갱신 */

    if (n != NULL)
        chunk_set_prev_free(n, p);   /* next의 prev를 c의 prev로 */
}

/*--------------------------------------------------------------------*/
/* fl_insert_front: 블록 c를 free list 맨 앞(head)에 삽입한다.         */
/*                                                                    */
/* LIFO 방식: 가장 최근에 free된 블록이 맨 앞에 온다.                   */
/* O(1) 삽입. c를 FREE 상태로 설정하고 포인터를 연결한다.               */
/*--------------------------------------------------------------------*/
static void
fl_insert_front(Chunk_T c)
{
    chunk_set_status(c, CHUNK_FREE);
    chunk_set_prev_free(c, NULL);          /* 새 head이므로 prev = NULL */
    chunk_set_next_free(c, s_free_head);   /* 기존 head를 next로 */

    if (s_free_head != NULL)
        chunk_set_prev_free(s_free_head, c); /* 기존 head의 prev를 c로 */

    s_free_head = c;                       /* c가 새로운 head */
}

/*--------------------------------------------------------------------*/
/* split_block: free 블록 c를 분할하여 뒷부분을 할당 블록으로 반환.     */
/*                                                                    */
/* [분할 전]                                                           */
/*   c: [Header|......payload(free)........|Footer]                   */
/*                                                                    */
/* [분할 후]                                                           */
/*   c(축소): [Header|.payload.|Footer]  ← free, 남은 부분            */
/*   alloc:                        [Header|payload|Footer] ← 할당됨  */
/*                                                                    */
/* 앞부분을 free로 남기고 뒷부분을 할당하는 이유:                       */
/*   → free 블록 c의 free list 포인터를 유지할 수 있어서                */
/*     free list를 재조정할 필요가 적다.                                */
/*                                                                    */
/* 전제조건: 남는 부분이 MIN_BLOCK_SPAN(3) 이상이어야 분할 가능.        */
/*--------------------------------------------------------------------*/
static Chunk_T
split_block(Chunk_T c, size_t need_units)
{
    Chunk_T alloc;
    int old_span = chunk_get_span_units(c);
    int alloc_span = (int)(OVERHEAD_UNITS + need_units); /* 할당 블록 크기 */
    int remain_span = old_span - alloc_span;             /* 남은 블록 크기 */

    assert(chunk_get_status(c) == CHUNK_FREE);
    assert(remain_span >= MIN_BLOCK_SPAN);  /* 남은 부분이 유효한 블록이 되는지 */

    /* 앞부분(free)을 축소 */
    chunk_set_span_units(c, remain_span);

    /* 뒷부분에 할당 블록 생성 */
    alloc = chunk_get_adjacent(c, s_heap_lo, s_heap_hi);
    chunk_set_span_units(alloc, alloc_span);
    chunk_set_status(alloc, CHUNK_USED);

    return alloc;
}

/*--------------------------------------------------------------------*/
/* grow_heap: 힙을 확장하여 새 블록을 생성한다.                         */
/*                                                                    */
/* 1) sbrk()로 OS에 메모리 요청 (최소 MIN_GROW_UNITS만큼)              */
/* 2) 새 블록을 생성하고 USED로 표시                                    */
/* 3) 바로 이전 물리적 블록이 free이면 합병 (sbrk 연속 호출 최적화)     */
/* 4) 합병된/새 블록을 free list에 삽입                                 */
/*                                                                    */
/* 실패 시 NULL 반환.                                                   */
/*--------------------------------------------------------------------*/
static Chunk_T
grow_heap(size_t need_units)
{
    Chunk_T c;
    Chunk_T prev_phys;
    size_t grow_data;
    size_t grow_span;

    /* 최소 MIN_GROW_UNITS(1024) 이상 요청하여 sbrk 호출 횟수를 줄임 */
    grow_data = (need_units < MIN_GROW_UNITS)
                ? MIN_GROW_UNITS : need_units;
    grow_span = OVERHEAD_UNITS + grow_data;

    c = (Chunk_T)sbrk((int)(grow_span * CHUNK_UNIT));
    if (c == (Chunk_T)-1)
        return NULL;

    s_heap_hi = sbrk(0);  /* 힙 끝 주소 갱신 */

    chunk_set_span_units(c, (int)grow_span);
    chunk_set_status(c, CHUNK_USED);

    /* 이전 물리적 블록이 free이면 합병하여 단편화 방지 */
    prev_phys = chunk_get_prev_adjacent(c, s_heap_lo);
    if (prev_phys != NULL
        && chunk_get_status(prev_phys) == CHUNK_FREE)
    {
        fl_remove(prev_phys);  /* 이전 블록을 free list에서 제거 */
        chunk_set_span_units(
            prev_phys,
            chunk_get_span_units(prev_phys)
            + chunk_get_span_units(c));  /* 두 블록을 하나로 합침 */
        c = prev_phys;  /* 합쳐진 블록이 새 c */
    }

    fl_insert_front(c);  /* free list에 삽입 */

    assert(check_heap_validity());
    return c;
}

/*--------------------------------------------------------------------*/
/* find_fit: First-fit 방식으로 free list에서 적합한 블록을 찾는다.     */
/*                                                                    */
/* free list를 처음부터 순회하며 payload가 need_units 이상인            */
/* 첫 번째 블록을 반환한다. 없으면 NULL.                                */
/*                                                                    */
/* ★ 이 함수가 heapmgr1의 약점: worst-case O(n) 순회.                  */
/*    heapmgr2에서는 bins + bitmap으로 이 문제를 해결한다.              */
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
/* allocate_from: free 블록 c에서 need_units만큼 할당한다.              */
/*                                                                    */
/* 두 가지 경우:                                                       */
/*   1) 분할 가능: 남는 공간이 MIN_BLOCK_SPAN 이상이면 블록을 쪼갠다   */
/*   2) 분할 불가: 블록 전체를 사용한다 (내부 단편화 발생하지만 허용)   */
/*--------------------------------------------------------------------*/
static void *
allocate_from(Chunk_T c, size_t need_units)
{
    size_t avail = payload_units(c);

    if (avail >= need_units + MIN_BLOCK_SPAN) {
        /* 분할: 뒷부분을 할당, 앞부분은 free로 유지 */
        c = split_block(c, need_units);
    }
    else {
        /* 전체 사용: free list에서 제거하고 USED로 변경 */
        fl_remove(c);
        chunk_set_status(c, CHUNK_USED);
    }
    return payload_from_header(c);
}

/*--------------------------------------------------------------------*/
/* heapmgr_malloc: size 바이트 이상의 메모리를 할당하여 반환한다.        */
/*                                                                    */
/* 동작 흐름:                                                          */
/*   1) size == 0이면 NULL 반환                                        */
/*   2) 최초 호출 시 heap_bootstrap()으로 힙 초기화                    */
/*   3) 요청 바이트를 unit 수로 변환                                    */
/*   4) find_fit()으로 free list에서 적합한 블록 탐색                   */
/*   5) 찾으면 allocate_from()으로 할당                                 */
/*   6) 못 찾으면 grow_heap()으로 힙 확장 후 할당                      */
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

    /* prev_free 포인터 저장을 위해 최소 1 unit 필요 */
    if (need_units < 1)
        need_units = 1;

    /* free list에서 적합한 블록 탐색 */
    cur = find_fit(need_units);
    if (cur != NULL) {
        void *result = allocate_from(cur, need_units);
        assert(check_heap_validity());
        return result;
    }

    /* 적합한 블록 없음 → 힙 확장 */
    cur = grow_heap(need_units);
    if (cur == NULL) {
        assert(check_heap_validity());
        return NULL;  /* 메모리 부족 */
    }

    {
        void *result = allocate_from(cur, need_units);
        assert(check_heap_validity());
        return result;
    }
}

/*--------------------------------------------------------------------*/
/* coalesce_neighbors: 블록 c의 물리적 이웃을 검사하여 합병한다.        */
/*                                                                    */
/* ★ Boundary Tag 활용의 핵심 ★                                       */
/*                                                                    */
/* 1단계: 다음(오른쪽) 이웃이 free이면 합병                             */
/*   c: [Header|payload|Footer][Header|payload(free)|Footer]          */
/*   →  [Header|...........확장된 payload...........|Footer]          */
/*                                                                    */
/* 2단계: 이전(왼쪽) 이웃이 free이면 합병                               */
/*   [Header|payload(free)|Footer][Header|payload|Footer] :c          */
/*   → [Header|...........확장된 payload...........|Footer]           */
/*   이때 c는 이전 블록의 header로 변경됨                               */
/*                                                                    */
/* 양방향 합병이 모두 O(1): boundary tag 덕분에 이전 블록의 위치를      */
/* footer에서 즉시 알 수 있고, 이중 연결 리스트이므로 fl_remove도 O(1). */
/*--------------------------------------------------------------------*/
static Chunk_T
coalesce_neighbors(Chunk_T c)
{
    Chunk_T neighbor;

    /* 다음 물리적 이웃과 합병 */
    neighbor = chunk_get_adjacent(c, s_heap_lo, s_heap_hi);
    if (neighbor != NULL
        && chunk_get_status(neighbor) == CHUNK_FREE)
    {
        fl_remove(neighbor);  /* 이웃을 free list에서 제거 */
        chunk_set_span_units(
            c,
            chunk_get_span_units(c)
            + chunk_get_span_units(neighbor));  /* c에 이웃 크기를 합산 */
    }

    /* 이전 물리적 이웃과 합병 (boundary tag로 O(1) 접근) */
    neighbor = chunk_get_prev_adjacent(c, s_heap_lo);
    if (neighbor != NULL
        && chunk_get_status(neighbor) == CHUNK_FREE)
    {
        fl_remove(neighbor);  /* 이웃을 free list에서 제거 */
        chunk_set_span_units(
            neighbor,
            chunk_get_span_units(neighbor)
            + chunk_get_span_units(c));  /* 이웃에 c 크기를 합산 */
        c = neighbor;  /* 합쳐진 블록의 header는 이전 이웃의 header */
    }

    return c;  /* 최종 합쳐진 블록 반환 */
}

/*--------------------------------------------------------------------*/
/* heapmgr_free: 이전에 할당된 메모리 블록을 해제한다.                   */
/*                                                                    */
/* 동작 흐름:                                                          */
/*   1) p가 NULL이면 아무것도 하지 않음                                 */
/*   2) payload 포인터에서 header를 역산                                */
/*   3) 해당 블록이 실제로 USED인지 assert로 검증                       */
/*   4) coalesce_neighbors()로 인접 free 블록과 합병                    */
/*   5) fl_insert_front()로 free list 맨 앞에 삽입                     */
/*--------------------------------------------------------------------*/
void
heapmgr_free(void *p)
{
    Chunk_T c;

    if (p == NULL)
        return;

    assert(check_heap_validity());

    c = header_from_payload(p);
    assert(chunk_get_status(c) != CHUNK_FREE);  /* 이중 free 방지 */

    c = coalesce_neighbors(c);  /* 인접 free 블록과 합병 */
    fl_insert_front(c);         /* free list 맨 앞에 삽입 (LIFO) */

    assert(check_heap_validity());
}
