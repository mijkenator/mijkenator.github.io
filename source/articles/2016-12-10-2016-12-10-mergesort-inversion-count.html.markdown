---
title: Merge sort - inversions count
date: 2016-12-10 23:15 UTC
tags: algorithm, elixir, erlang, python, merge sort
layout: post
---

In an array, arr = [a0, a1, ..., aN-1] , the elements at indices i and j  (where i < j) form an inversion if aI > aJ. In other words, inverted elements  and  are considered to be "out of order". To correct an inversion, we can swap adjacent elements.

READMORE

For example, consider arr = [2,4,1]. It has two inversions: (4,1) and (2,1). To sort the array, we must perform the following two swaps to correct the inversions:
[2,4,1] -> [2,1,4] -> [1,2,4].

<br>

[Wikipedia article about merge sort algorithm.](https://en.wikipedia.org/wiki/Merge_sort)<br>

Python implementation

```python
def count_inversions(lst):
    return merge_count_inversion(lst)[1]

def merge_count_inversion(lst):
    if len(lst) <= 1:
        return lst, 0
    middle = int( len(lst) / 2 )
    left, a = merge_count_inversion(lst[:middle])
    right, b = merge_count_inversion(lst[middle:])
    result, c = merge_count_split_inversion(left, right)
    return result, (a + b + c)

def merge_count_split_inversion(left, right):
    result = []
    count = 0
    i, j = 0, 0
    left_len = len(left)
    while i < left_len and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            count += left_len - i
            j += 1
    result += left[i:]
    result += right[j:]
    return result, count
```

Erlang Implementation

```erlang
inv_count(L) when length(L) =< 1 -> {0, L};
inv_count(L) ->
    {L1, L2}   = lists:split(length(L) div 2, L),
    {A, Left}  = inv_count(L1),
    {B, Right} = inv_count(L2),
    {C, Res}   = mcsi(Left, Right),
    {A+B+C, Res}.

mcsi(L, R) -> mcsi_i(length(L),0,[],L,R).

mcsi_i(_, C, Res, [], Right) -> {C, Res ++ Right};
mcsi_i(_, C, Res, Left, [])  -> {C, Res ++ Left};
mcsi_i(N, C, Res, [HL|TL] = Left, [HR|TR] = Right) ->
    case HL =< HR of 
        true  -> mcsi_i(N-1, C, Res ++ [HL], TL, Right);
        false -> mcsi_i(N, C+N, Res ++ [HR], Left, TR)
    end.
```


