---
title: Project Euler problem 46
date: 2016-04-12 18:20 UTC
tags: project euler, erlang, python
layout: post
---

<b>Goldbach's other conjecture</b>

It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

<pre>
        9  = 7  + 2×1^2
        15 = 7  + 2×2^2
        21 = 3  + 2×3^2
        25 = 7  + 2×3^2
        27 = 19 + 2×2^2
        33 = 31 + 2×1^2
</pre>

READMORE

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

[Link to original description](https://projecteuler.net/problem=46)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p46)<br>

## Python version

```python
#!/usr/bin/python3

import prime

MAX = 10000
prime._refresh(MAX)
squares = dict.fromkeys((x*x for x in range(1, MAX)), 1)

for x in range(35, MAX, 2):
    if not prime.isprime(x):
        is_goldbach = 0
        for p in prime.prime_list:
            if p >= x: break
            key = (x-p)/2
            if key in squares:
                is_goldbach = 1
                break
        if not is_goldbach:
            print(x)
            break


```

## Erlang version

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p46
% vim:syn=erlang

-mode(compile).
-define(MAX, 10000).

main(_) -> 
    get_primes(?MAX),
    get_squares(?MAX),
    io:format("Answer ~p ~n", [t46(35)]).

t46(N) when N < ?MAX ->
    case is_prime(N) of
        true -> t46(N+2)
        ;_   -> 
                case is_goldbach(N, get("primes")) of
                    true -> t46(N+2)
                    ;_   -> N
                end
    end;
t46(N) -> {stop, N}.

is_goldbach(N, [H|T]) when N >= H ->
   case (N-H) rem 2 of
        0 ->
           case ets:lookup(squares, (N-H) div 2) of
                [] -> is_goldbach(N, T)
                ;_ -> true
           end;
        _ -> is_goldbach(N, T) 
    end;
is_goldbach(_, _) -> false.

is_prime(N) ->
    case ets:lookup(prim, N) of
        [] -> false
        ;_ -> true
    end.

get_squares(N) ->
    ets:new(squares, [public, named_table, {write_concurrency, true} ]),
    lists:foreach(fun(E)-> ets:insert(squares, {E*E,1}) end, lists:seq(1, N)).
%----------------------------------------------- prime generator from Project Euler 10 (version 5 ---------------------------)
get_primes(N) ->
    ets:new(comp, [public, named_table, {write_concurrency, true} ]),
    ets:new(prim, [public, named_table, {write_concurrency, true}]),
    composite_mc(N),
    primes_mc(N),
    lists:sort([P || {P,_} <-ets:tab2list(prim)]).

primes_mc(N) ->
    case erlang:system_info(schedulers) of
        1 -> primes(N);
        C -> launch_primes(lists:seq(1,C), C, N, N div C)
    end.
launch_primes([1|T], C, N, R) -> P = self(), spawn(fun()-> primes(2,R), P ! {ok, prm} end), launch_primes(T, C, N, R);
launch_primes([H|[]], C, N, R)-> P = self(), spawn(fun()-> primes(R*(H-1)+1,N), P ! {ok, prm} end), wait_primes(C);
launch_primes([H|T], C, N, R) -> P = self(), spawn(fun()-> primes(R*(H-1)+1,R*H), P ! {ok, prm} end), launch_primes(T, C, N, R).

wait_primes(0) -> ok;
wait_primes(C) ->
    receive
        {ok, prm} -> wait_primes(C-1)
    after 1000    -> wait_primes(C)
    end.

primes(N) -> primes(2, N).
primes(I,N) when I =< N ->
    case ets:lookup(comp, I) of
        [] -> ets:insert(prim, {I,1})
        ;_ -> ok
    end,
    primes(I+1, N);
primes(I,N) when I > N -> ok.


composite_mc(N) -> composite_mc(N,2,round(math:sqrt(N)),erlang:system_info(schedulers)).
composite_mc(N,I,M,C) when I =< M, C > 0 ->
    C1 = case ets:lookup(comp, I) of
        [] -> comp_i_mc(I*I, I, N), C-1
        ;_ -> C
    end,
    composite_mc(N,I+1,M,C1);
composite_mc(_,I,M,_) when I > M -> ok;
composite_mc(N,I,M,0) ->
    receive
        {ok, cim} -> composite_mc(N,I,M,1)
    after 1000    -> composite_mc(N,I,M,0)
    end.

comp_i_mc(J, I, N) -> 
    Parent = self(),
    spawn(fun() ->
        comp_i(J, I, N),
        Parent ! {ok, cim}
    end).

comp_i(J, I, N) when J =< N -> ets:insert(comp, {J, 1}), comp_i(J+I, I, N);
comp_i(J, _, N) when J > N -> ok.


```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/04/12/erlang-python-project-euler-46/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep46'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
};
*/
(function() { // DON'T EDIT BELOW THIS LINE
var d = document, s = d.createElement('script');

s.src = '//mijkenator.disqus.com/embed.js';

s.setAttribute('data-timestamp', +new Date());
(d.head || d.body).appendChild(s);
})();
</script>
<noscript>Please enable JavaScript to view the <a href="https://disqus.com/?ref_noscript" rel="nofollow">comments powered by Disqus.</a></noscript>

