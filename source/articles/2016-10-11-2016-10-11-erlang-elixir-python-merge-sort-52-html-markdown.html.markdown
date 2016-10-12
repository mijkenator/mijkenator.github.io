---
title: Merge sort with erlang, elixir, python
date: 2016-10-11 20:34 UTC
tags: algorithm, elixir, erlang, python, merge sort
layout: post
---

[Wikipedia article about merge sort algorithm.](https://en.wikipedia.org/wiki/Merge_sort)<br>

READMORE

Erlang implementation

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p9
% vim:syn=erlang

-mode(compile).

main(_) ->
    {A,B,C} = erlang:timestamp(),
    random:seed(A,B,C),
    L = [random:uniform(10000)||_<-lists:seq(1,100000)],
    m(L),
    io:format("Done").

%--------------- merge sort implementation --------------------------

m([L]) -> [L]; 
m(L)   ->
    {L1,L2} = lists:split(length(L) div 2, L),
    merge(m(L1), m(L2)).

merge(L1, L2)    -> merge(L1, L2, []).
merge([], L2, A) -> A++L2;
merge(L1, [], A) -> A++L1;
merge([H1|T1], [H2|T2], A) when H2>=H1 -> merge(T1, [H2|T2], A++[H1]);
merge([H1|T1], [H2|T2], A) when H1>H2  -> merge([H1|T1], T2, A++[H2]).

```

erlang has efficient built-in merge function for sorted lists and we can rewrite implementation:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p9
% vim:syn=erlang

-mode(compile).

main(_) ->
    {A,B,C} = erlang:timestamp(),
    random:seed(A,B,C),
    L = [random:uniform(10000)||_<-lists:seq(1,5000000)],
    M = m(L),
    io:format("Done ~p", [M]).

%--------------- merge sort implementation --------------------------

m([L]) -> [L]; 
m(L)   ->
    {L1,L2} = lists:split(length(L) div 2, L),
    lists:merge(m(L1), m(L2)).

```

almost same implementation with Elixir:

```erlang
defmodule Sort do

    def m_s([l]), do: [l]
    def m_s(list) do
        {l, r} = Enum.split(list, div(length(list), 2))        
        :lists.merge(m_s(l), m_s(r))
    end

end

```

Merge sort algorith easily parallelizes. This is erlang implementation of merge sort, it using same number of concurrent processes as number of
system processors:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p9
% vim:syn=erlang

-mode(compile).

main(_) ->
    {A,B,C} = erlang:timestamp(),
    random:seed(A,B,C),
    L = [random:uniform(10000)||_<-lists:seq(1,5000000)],
    _M = m(L, erlang:system_info(schedulers) ),
    io:format("Done ~p", ["M"]).

%---------------parallel merge sort implementation --------------------------

m([L],_) -> [L]; 
m(L, N) when N > 1  -> 
    {L1,L2} = lists:split(length(L) div 2, L),
    {Parent, Ref} = {self(), make_ref()},
    spawn(fun()-> Parent ! {l1, Ref, m(L1, N-2)} end), 
    spawn(fun()-> Parent ! {l2, Ref, m(L2, N-2)} end), 
    {L1R, L2R} = receive_results(Ref, undefined, undefined),
    lists:merge(L1R, L2R);
m(L, _) -> {L1,L2} = lists:split(length(L) div 2, L), lists:merge(m(L1, 0), m(L2, 0)).

receive_results(Ref, L1, L2) ->
    receive
        {l1, Ref, L1R} when L2 == undefined -> receive_results(Ref, L1R, L2);
        {l2, Ref, L2R} when L1 == undefined -> receive_results(Ref, L1, L2R);
        {l1, Ref, L1R} -> {L1R, L2};
        {l2, Ref, L2R} -> {L1, L2R}
    after 5000 -> receive_results(Ref, L1, L2)
    end.

```

so, i have virtual machine with 2 cores and this are measurements for regular and parallel merge sorts erlang implementations (sorting 5 million list of integers):

```bash
vagrant@mblog:~/pr_euler/algorithms/mergesort$ time ./merge2.erl
real    0m9.124s
user    0m7.708s
sys     0m0.408s
vagrant@mblog:~/pr_euler/algorithms/mergesort$ time ./merge3.erl
real    0m5.245s
user    0m6.560s
sys     0m0.444s
vagrant@mblog:~/pr_euler/algorithms/mergesort$
```

and Python recursive implementation of merge sort:

```python
#!/usr/bin/python3

import random
from heapq import merge

def m(l):
    if len(l) <= 1: return l
    middle = len(l) // 2
    left, right = l[:middle], l[middle:]
    left  = m(left)
    right = m(right) 

    return list(merge(left, right))

l = [random.randint(1,10000) for _ in range(1,5000000)]

m(l)
print("Done")

```

my python recursive implementation very slow :

```bash
vagrant@mblog:~/pr_euler/algorithms/mergesort$ time ./merge1.py
Done

real    0m38.014s
user    0m37.480s
sys     0m0.252s
```

