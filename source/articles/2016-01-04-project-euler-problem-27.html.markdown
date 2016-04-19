---
title: Project Euler Problem 27
date: 2016-01-04 21:07 UTC
tags: project euler, erlang, python
layout: post
---

<b>Quadratic primes.</b>

Euler discovered the remarkable quadratic formula:

<p style="text-align:center">
n² + n + 41
</p>

READMORE

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 40² + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

<pre>
n² + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |−4| = 4
</pre>

Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

[Link to original description](https://projecteuler.net/problem=27)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p27)<br>

## Python version
```python
#!/usr/bin/python

import prime

max_pair = (0,0,0)
for a in xrange(-999, 1000):
    for b in xrange(max(2, 1-a), 1000): # b >= 2, a + b + 1 >= 2
        n, count = 0, 0
        while True:
            v = n*n + a*n + b
            prime._refresh(v)
            if prime.isprime(v): count = count + 1
            else: break
            n = n + 1
        if count > max_pair[2]:
            max_pair = (a,b,count)

print max_pair[0] * max_pair[1]

```

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p27
% vim:syn=erlang

-mode(compile).

main(_) -> 
    io:format("Answer ~p ~n", [test27()]).

test27() ->
    P = get_primes(1001),
    put("primes", P),
    {_,A,B} = lists:last(lists:usort([t27(X,P,{0,0,0}) || X <- lists:seq(-1000, 1000)])),
    A*B.

t27(_,[], Ac)               -> Ac;
t27(A, [B|T], {N,_,_} = Ac) ->
    case {calc(0,A,B),calc(0,A,-B)} of
        {N1,N2} when N1 > N2, N1 > N -> t27(A, T, {N1, A, B});
        {N1,N2} when N2 > N1, N2 > N -> t27(A, T, {N2, A, B})
        ;_ -> t27(A, T, Ac)
    end.

calc(N, A, B) ->
    case is_prime(N*N + A*N + B) of
        true -> calc(N+1, A, B)
        ;_   -> N
    end.

is_prime(N) -> is_prime(N, 2).
is_prime(N, _) when N < 1000 -> lists:member(N, get("primes"));
is_prime(N, M) when M > N div 2   -> true;
is_prime(N, M) when N rem M =:= 0 -> false;
is_prime(N, M) -> is_prime(N, M + 1).

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
    this.page.url = '2016/01/04/project-euler-problem-27/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep27'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


