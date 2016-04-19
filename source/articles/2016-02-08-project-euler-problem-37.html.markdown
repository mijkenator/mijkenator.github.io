---
title: Project Euler Problem 37
date: 2016-02-08 21:12 UTC
tags: project euler, erlang, python
layout: post
---

<b>Truncatable primes</b>

The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

READMORE

[Link to original description](https://projecteuler.net/problem=37)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p37)<br>

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p37
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:sum(lists:sublist([P||P<-prime(1000000),
        P > 7,
        trunc_left_Prime( integer_to_list(P)), 
        trunc_right_Prime(integer_to_list(P))],1,11)),
    io:format("Answer ~p ~n", [ Answer ]).
 
trunc_left_Prime([_|T]) when length(T) > 1 ->
    case is_prime(list_to_integer(T)) of
        true  -> trunc_left_Prime(T);
        false -> false
    end;
trunc_left_Prime([_|H]) -> is_prime(list_to_integer(H)).

trunc_right_Prime(H) when length(H) > 1 ->
    {T,_} = lists:split(length(H)-1,H),
    case is_prime(list_to_integer(T)) of
        true  -> trunc_right_Prime(T);
        false -> false
    end;
trunc_right_Prime(H) -> is_prime(list_to_integer(H)).

%------------------------------------- prime library from problem 10------------
is_prime(X) ->
    case ets:lookup(prim, X) of
        [] -> false
        ;_ -> true
    end.

prime(N) ->    
    ets:new(comp, [public, named_table, {write_concurrency, true} ]),
    ets:new(prim, [public, named_table, {write_concurrency, true}]),
    composite_mc(N),
    primes_mc(N),
    lists:sort([X||{X,_}<- ets:tab2list(prim)]).

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

## Python version
```python
#!/usr/bin/python

import math, prime

def is_left_truncatable(i):
    while True:
        if prime.isprime(i):
            if i > 10: i = int(''.join(list(str(i))[1::]))
            else: return True
        else: return False

def is_right_truncatable(i):
    while True:
        if prime.isprime(i):
            if i > 10: 
                a = list(str(i))
                a.pop()
                i = int(''.join(a))
            else: return True
        else: return False

def p37():
    start,answer,count = 1,0,0
    while True:
        for i,v in enumerate(prime.prime_list[start+1::], start=start+1):
            if v > 7 and is_left_truncatable(v) and is_right_truncatable(v):
                answer += v
                count += 1
                if count == 11: return answer
        start = i
        prime._refresh(v*2)

print "Answer %s" % p37()



```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/08/project-euler-problem-37/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep37'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


