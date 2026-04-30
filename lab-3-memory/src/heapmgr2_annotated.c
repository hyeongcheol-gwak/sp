/*====================================================================*/
/* heapmgr2.c (한글 주석 버전)                                         */
/*                                                                    */
/* Segregated-fit bins + bitmap을 사용하는 고성능 힙 매니저.            */
/*                                                                    */
/* heapmgr1 대비 핵심 개선점:                                          */
/*   heapmgr1은 단일 free list → worst-case O(n) 탐색                 */
/*   heapmgr2는 크기별 15개 bin + bitmap → O(1) 수준 탐색              */
/*                                                                    */
/* 설계 요약:                                                          */
/*   - 블록 구조: heapmgr1과 동일 (Header + Payload + Footer)         */
/*   - Free list: 크기별 15개 bin, 각 bin은 이중 연결 리스트            */
/*   - Bitmap: 비어있지 않은 bin을 비트로 표시 → 빈 bin 스킵           */
/*   - 할당: 해당 bin에서 first-fit, 없으면 bitmap으로 큰 bin 탐색     */
/*   - 합병: boundary tag로 O(1) (heapmgr1과 동일)                    */
/*====================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "chunk.h"

#define FALSE 0
#define TRUE  1

enum { OVERHEAD_UNITS = 2 };
enum { MIN_GROW_UNITS = 1024 };

/*--------------------------------------------------------------------*/
/* NUM_BINS: 15개의 크기 클래스 bin.                                    */
/*                                                                    */
/* Bin 할당 (payload unit 기준):                                       */
/*   Bin 0: [1]        Bin 1: [2]        Bin 2: [3]       Bin 3: [4]  */
/*   Bin 4: [5-8]      Bin 5: [9-16]     Bin 6: [17-32]              */
/*   Bin 7: [33-64]    Bin 8: [65-128]   Bin 9: [129-256]            */
/*   Bin 10: [257-512] Bin 11: [513-1024] Bin 12: [1025-2048]        */
/*   Bin 13: [2049-4096]  Bin 14: [4097 이상]                         */
/*                                                                    */
/* 작은 크기(1~4)는 정확한 bin, 큰 크기는 2의 거듭제곱 범위별 bin.      */
/* 자주 사용되는 작은 크기의 할당을 빠르게 처리하기 위한 설계.           */
/*--------------------------------------------------------------------*/
enum { NUM_BINS = 15 };

/*--------------------------------------------------------------------*/
/* 모듈 전역 상태                                                      */
/*   s_bins[i]: i번째 bin의 free list head                             */
/*   s_bitmap: 비어있지 않은 bin을 비트로 표시 (bit i = bin i가 비었는지)*/
/*             s_bitmap & (1 << i) != 0 이면 bin i에 블록이 있음       */
/*--------------------------------------------------------------------*/
static Chunk_T s_bins[NUM_BINS];
static unsigned int s_bitmap = 0;
static void *s_heap_lo = NULL;
static void *s_heap_hi = NULL;

/*--------------------------------------------------------------------*/
/* get_bin_index: payload unit 수에 해당하는 bin 인덱스를 반환한다.     */
/*                                                                    */
/* 크기 1~4: 정확한 매핑 (bin 0~3)                                     */
/* 크기 5 이상: 2의 거듭제곱 범위로 매핑 (bin 4~14)                     */
/*--------------------------------------------------------------------*/
static int
get_bin_index(size_t pu)
{
    if (pu <= 4)   return (int)(pu - 1);  /* 정확한 크기 bin */
    if (pu <= 8)   return 4;
    if (pu <= 16)  return 5;
    if (pu <= 32)  return 6;
    if (pu <= 64)  return 7;
    if (pu <= 128) return 8;
    if (pu <= 256) return 9;
    if (pu <= 512) return 10;
    if (pu <= 1024) return 11;
    if (pu <= 2048) return 12;
    if (pu <= 4096) return 13;
    return 14;  /* 4097 이상: catch-all bin */
}

/*--------------------------------------------------------------------*/
/* payload_units: 블록의 payload unit 수 (span - 오버헤드).             */
/*--------------------------------------------------------------------*/
static size_t
payload_units(Chunk_T c)
{
    return (size_t)(chunk_get_span_units(c) - OVERHEAD_UNITS);
}

/*--------------------------------------------------------------------*/
/* check_heap_validity: 디버그 모드 전용 무결성 검사.                    */
/* heapmgr1과 유사하지만 단일 free list 대신 모든 bin을 순회한다.       */
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

    /* 모든 물리적 블록 순회 */
    for (w = (Chunk_T)s_heap_lo;
         w && w < (Chunk_T)s_heap_hi;
         w = chunk_get_adjacent(w, s_heap_lo, s_heap_hi))
    {
        if (!chunk_is_valid(w, s_heap_lo, s_heap_hi))
            return FALSE;
    }

    /* 모든 bin의 free list를 순회하며 검사 */
    for (i = 0; i < NUM_BINS; i++) {
        for (w = s_bins[i]; w != NULL;
             w = chunk_get_next_free(w))
        {
            Chunk_T adj;
            if (chunk_get_status(w) != CHUNK_FREE) {
                fprintf(stderr, "Non-free in bin %d\n", i);
                return FALSE;
            }
            if (!chunk_is_valid(w, s_heap_lo, s_heap_hi))
                return FALSE;
            /* 합병 누락 검사 */
            adj = chunk_get_adjacent(w, s_heap_lo, s_heap_hi);
            if (adj != NULL
                && chunk_get_status(adj) == CHUNK_FREE)
            {
                fprintf(stderr, "Uncoalesced adjacent\n");
                return FALSE;
            }
        }
    }
    return TRUE;
}
#endif

/*--------------------------------------------------------------------*/
/* bytes_to_units / header_from_payload / payload_from_header          */
/* heapmgr1과 동일한 유틸리티 함수들.                                   */
/*--------------------------------------------------------------------*/
static size_t
bytes_to_units(size_t bytes)
{
    return (bytes + CHUNK_UNIT - 1) / CHUNK_UNIT;
}

static Chunk_T
header_from_payload(void *p)
{
    return (Chunk_T)((char *)p - CHUNK_UNIT);
}

static void *
payload_from_header(Chunk_T c)
{
    return (void *)((char *)c + CHUNK_UNIT);
}

/*--------------------------------------------------------------------*/
/* heap_bootstrap: 힙과 모든 bin을 초기화한다.                          */
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
/* bin_remove: 블록 c를 해당 bin에서 O(1)에 제거한다.                   */
/*                                                                    */
/* 1) 이중 연결 리스트에서 prev/next를 연결하여 c를 분리                */
/* 2) bin이 비게 되면 bitmap에서 해당 비트를 클리어                      */
/*    → s_bitmap &= ~(1u << idx)                                      */
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
        s_bins[idx] = n;  /* c가 bin의 head였으면 head 갱신 */

    if (n != NULL)
        chunk_set_prev_free(n, p);

    /* bin이 비었으면 bitmap 비트 클리어 */
    if (s_bins[idx] == NULL)
        s_bitmap &= ~(1u << idx);
}

/*--------------------------------------------------------------------*/
/* bin_insert: 블록 c를 해당 크기의 bin 맨 앞에 삽입한다.               */
/*                                                                    */
/* 1) c의 payload 크기로 적절한 bin 인덱스를 계산                       */
/* 2) c를 해당 bin의 head에 LIFO 방식으로 삽입                         */
/* 3) bitmap에서 해당 비트를 설정                                       */
/*    → s_bitmap |= (1u << idx)                                       */
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
    s_bitmap |= (1u << idx);  /* bin에 블록이 있음을 표시 */
}

/*--------------------------------------------------------------------*/
/* split_block: free 블록을 분할한다. heapmgr1과 유사하지만             */
/* bin 재배치 최적화가 추가되었다.                                       */
/*                                                                    */
/* ★ 최적화: 분할 후 남은 부분의 bin 인덱스가 변하지 않으면             */
/*   bin_remove + bin_insert를 생략하고 span만 업데이트한다.            */
/*   → 불필요한 리스트 조작을 피해서 속도 향상                          */
/*                                                                    */
/* bin 인덱스가 바뀌면 기존 bin에서 제거 후 새 bin에 삽입해야 한다.      */
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

    /* 분할 전후의 bin 인덱스 비교 */
    old_bin = get_bin_index((size_t)(old_span - OVERHEAD_UNITS));
    new_bin = get_bin_index((size_t)(remain_span - OVERHEAD_UNITS));

    if (old_bin == new_bin) {
        /* bin이 같으면 span만 줄이면 됨 (O(1), 리스트 조작 없음) */
        chunk_set_span_units(c, remain_span);
    }
    else {
        /* bin이 바뀌면 재배치 필요 */
        bin_remove(c);
        chunk_set_span_units(c, remain_span);
        bin_insert(c);
    }

    /* 뒷부분에 할당 블록 생성 */
    alloc = chunk_get_adjacent(c, s_heap_lo, s_heap_hi);
    chunk_set_span_units(alloc, alloc_span);
    chunk_set_status(alloc, CHUNK_USED);

    return alloc;
}

/*--------------------------------------------------------------------*/
/* grow_heap: 힙 확장. heapmgr1과 동일한 로직이지만                     */
/* fl_remove/fl_insert_front 대신 bin_remove/bin_insert을 사용한다.    */
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

    /* 이전 블록이 free이면 합병 */
    prev_phys = chunk_get_prev_adjacent(c, s_heap_lo);
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
/* find_fit: 적합한 free 블록을 찾는다.                                 */
/*                                                                    */
/* ★ heapmgr2의 핵심: bitmap을 활용한 빠른 탐색 ★                     */
/*                                                                    */
/* 1단계: 요청 크기에 맞는 bin에서 first-fit 탐색                       */
/*   → 해당 bin의 블록들은 비슷한 크기이므로 빠르게 찾을 수 있음        */
/*                                                                    */
/* 2단계: 해당 bin에서 못 찾으면 bitmap으로 다음 비어있지 않은 bin 탐색  */
/*   → mask = s_bitmap & ~((1u << (idx+1)) - 1)                       */
/*     이렇게 하면 idx보다 큰 bin들의 비트만 남김                       */
/*   → mask의 최하위 set bit가 가장 작은 적합 bin                      */
/*   → 더 큰 bin의 블록은 반드시 요청 크기보다 크므로 첫 블록 바로 반환  */
/*                                                                    */
/* 시간 복잡도:                                                        */
/*   - 해당 bin 탐색: 보통 O(1)~O(k), k는 bin 내 블록 수               */
/*   - bitmap 탐색: O(1) (비트 연산)                                    */
/*   - 전체: amortized O(1)                                            */
/*                                                                    */
/* heapmgr1의 find_fit은 전체 free list O(n) 순회였으므로               */
/* worst 테스트에서 극적인 성능 차이가 발생한다.                         */
/*--------------------------------------------------------------------*/
static Chunk_T
find_fit(size_t need_units)
{
    int idx = get_bin_index(need_units);
    unsigned int mask;
    Chunk_T cur;

    /* 1단계: 정확한 bin에서 탐색 */
    for (cur = s_bins[idx]; cur != NULL;
         cur = chunk_get_next_free(cur))
    {
        if (payload_units(cur) >= need_units)
            return cur;
    }

    /* 2단계: bitmap으로 더 큰 bin 탐색 */
    mask = s_bitmap & ~((1u << (idx + 1)) - 1);
    if (mask == 0)
        return NULL;  /* 모든 큰 bin이 비어있음 */

    /* 최하위 set bit 찾기 = 가장 작은 적합 bin */
    idx = 0;
    while ((mask & 1u) == 0) {
        mask >>= 1;
        idx++;
    }

    /* 더 큰 bin의 첫 블록은 반드시 요청보다 크므로 바로 반환 */
    return s_bins[idx];
}

/*--------------------------------------------------------------------*/
/* allocate_from: free 블록에서 할당. heapmgr1과 동일한 로직이지만      */
/* bin_remove를 사용한다.                                               */
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
/* heapmgr_malloc: heapmgr1과 동일한 흐름.                              */
/* 내부적으로 find_fit이 bins+bitmap을 사용하는 것만 다르다.             */
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
        void *result = allocate_from(cur, need_units);
        assert(check_heap_validity());
        return result;
    }

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
/* coalesce_neighbors: heapmgr1과 동일한 양방향 합병.                   */
/* bin_remove를 사용하여 합병 대상 블록을 해당 bin에서 제거한다.         */
/*--------------------------------------------------------------------*/
static Chunk_T
coalesce_neighbors(Chunk_T c)
{
    Chunk_T neighbor;

    /* 다음 물리적 이웃 합병 */
    neighbor = chunk_get_adjacent(c, s_heap_lo, s_heap_hi);
    if (neighbor != NULL
        && chunk_get_status(neighbor) == CHUNK_FREE)
    {
        bin_remove(neighbor);
        chunk_set_span_units(
            c,
            chunk_get_span_units(c)
            + chunk_get_span_units(neighbor));
    }

    /* 이전 물리적 이웃 합병 */
    neighbor = chunk_get_prev_adjacent(c, s_heap_lo);
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
/* heapmgr_free: 블록을 해제하고 합병 후 적절한 bin에 삽입한다.         */
/*                                                                    */
/* heapmgr1과의 차이: fl_insert_front 대신 bin_insert 사용.             */
/* 합병 후 블록의 크기가 달라지므로 bin_insert가 자동으로 올바른         */
/* bin에 넣어준다.                                                      */
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
    bin_insert(c);  /* 합쳐진 크기에 맞는 bin에 삽입 */

    assert(check_heap_validity());
}
