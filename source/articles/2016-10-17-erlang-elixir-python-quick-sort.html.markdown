---
title: Quick sort with erlang, elixir, python
date: 2016-10-17 19:25 UTC
tags: algorithm, elixir, erlang, python, merge sort
layout: post
---

[Wikipedia article about merge sort algorithm.](https://en.wikipedia.org/wiki/Quicksort)<br>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/algorithms/quicksort)

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
    L = [random:uniform(10000)||_<-lists:seq(1,5000000)],
    {T,_L1} = timer:tc(fun()->qs(L) end),
    io:format("Done ~p ~n", [T/1000000]).
    %

%------------------ QUICKSORT ----------------------
qs([])    -> [];
qs([H|T]) ->
    qs([E||E<-T, E<H]) ++ [H] ++ qs([E||E<-T, H =< E]).
```

Almost same things with elixir:

```erlang

defmodule Sort do

    def qs([]), do: []
    def qs([h|t]) do
        l = for x <- t, x < h, do: x
        r = for x <- t, x >= h, do: x
        qs(l) ++ [h] ++ qs(r)
    end

end
```

and python implementation also with using list comprehensions:

```python

#!/usr/bin/python3
import random
from timeit import default_timer as timer

def qs(l):
    if len(l) == 0: return []
    return qs([x for x in l[1:] if x < l[0]]) + [l[0]] + qs([x for x in l[1:] if x >= l[0]])


l = [random.randint(1,10000) for _ in range(1,5000000)]
start=timer()
l1 = qs(l)
end=timer()
print("Done: %s" % str(end - start))

```

Erlang implementation of quicksort can be easily changed to distributed implementation. 
Implementation below using same erlang node and number of processes equal processor cores, but 
it can spawn new processes on different erlang nodes and different machines

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
    %io:format("Merge sort list: ~p ~n", [L]),
    %io:format("Merge sort list: ~p ~n", [qs(L, erlang:system_info(schedulers) )]).
    {T,_} = timer:tc(fun()->qs(L, erlang:system_info(schedulers)) end),
    io:format("Done ~p ~n", [T/1000000]).
    %

% ---------- distributed quicsort algorithm --------

qs([],_) -> [];
qs([H|T], N) when N > 1  -> 
    {Parent, Ref} = {self(), make_ref()},
    spawn(fun()-> Parent ! {l1, Ref, qs([E||E<-T, E<H], N-2)} end), 
    spawn(fun()-> Parent ! {l2, Ref, qs([E||E<-T, H =< E], N-2)} end), 
    {L1, L2} = receive_results(Ref, undefined, undefined), 
    L1 ++ [H] ++ L2;
qs([H|T],_) ->
    qs([E||E<-T, E<H],0) ++ [H] ++ qs([E||E<-T, H =< E],0).

receive_results(Ref, L1, L2) ->
    receive
        {l1, Ref, L1R} when L2 == undefined -> receive_results(Ref, L1R, L2);
        {l2, Ref, L2R} when L1 == undefined -> receive_results(Ref, L1, L2R);
        {l1, Ref, L1R} -> {L1R, L2};
        {l2, Ref, L2R} -> {L1, L2R}
    after 5000 -> receive_results(Ref, L1, L2)
    end.

```

Some speed measurements:

```bash
vagrant@mblog:~/pr_euler/algorithms/quicksort$ ./qs1.erl
Done 20.498445
vagrant@mblog:~/pr_euler/algorithms/quicksort$ ./qs2.erl
Done 15.467243
ivagrant@mblog:~/pr_euler/algorithms/quicksort$ ./qs1.py
Done: 192.65369251003722
```

Looks like recursion and list comprehensions in Python much slower Erlang.

