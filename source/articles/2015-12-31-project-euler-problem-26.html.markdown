---
title: Project Euler Problem 26
date: 2015-12-31 16:52 UTC
tags: project euler, erlang, python
layout: post
---

<b>Reciprocal cycles.</b>

A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:<br>
<pre>
1/2  =  0.5
1/3  =  0.(3)
1/4  =  0.25
1/5  =  0.2
1/6  =  0.1(6)
1/7  =  0.(142857)
1/8  =  0.125
1/9  =  0.(1)
1/10 =  0.1
</pre>

READMORE

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.<br>
Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.<br>

[Link to original description](https://projecteuler.net/problem=26)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p26)<br>

brutforce solution with erlang:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p26
% vim:syn=erlang

-mode(compile).

main(_) ->
    {_,Answer} = lists:last(lists:sort([ test26(X, []) || X <- lists:seq(1,10000)])),
    io:format("Answer: ~p ~n", [Answer]).

test26(N, [])      -> test26(N, [inc(1,N) rem N]);
test26(_, [0|_])   -> 0;
test26(N, [H|_]=L) ->
    D = inc(H,N) rem N,
    case lists:member(D, L) of
        true  -> {length(L) - indx(D, lists:reverse(L), 0), N}
        ;_ -> test26(N, [D | L])
    end.

indx(D, [D|_], I) -> I;
indx(D, [_|T], I) -> indx(D, T, I+1).

inc(H,N) when H >= N -> H;
inc(H,N) -> inc(H*10, N).
```

After that i found there is much effective algorithm for it. Good explanation [here](http://eli.thegreenplace.net/2009/02/25/project-euler-problem-26).
It is again about prime numbers and [multiplicative order of the denominator](http://mathworld.wolfram.com/MultiplicativeOrder.html).<br>

Here python realization of this algorithm (without primes):

```python
#!/usr/bin/python3

def recurring_cycle(n, d):
    for t in range(1, d):
        if 1 == pow(10, t, d):
            return (t,d)
    return (0,d)

Answer = sorted([recurring_cycle(1, i) for i in range(2,1001)], key = lambda tup: tup[0])[-1][1]

print("Answer %s" % Answer)

```

and here with primes:

```python
#!/usr/bin/python

def prime_sieve(n):
    sieve = [True] * (n/2)
    for i in xrange(3,int(n**0.5)+1,2):
        if sieve[i/2]:
            sieve[i*i/2::i] = [False] * ((n-i*i-1)/(2*i)+1)
    return [2] + [2*i+1 for i in xrange(1,n/2) if sieve[i]]

for d in prime_sieve(1000)[::-1]:
    period = 1
    while pow(10, period, d) != 1:
        period += 1
    if d-1 == period: break
 
print("Answer =", d)

```

After i tryed to do same thing with erlang:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p26
% vim:syn=erlang

-mode(compile).

main(_) -> 
    io:format("Answer ~p ~n", [test26()]).

test26() -> 
    element(2,lists:last(lists:sort(
        [{test26i(1,P),P} || P <- get_primes(1000), P=/=2,P=/=5]))).

test26i(I, P) ->
    case pow(10, I) rem P of
        1  -> I
        ;_ -> test26i(I+1, P)
    end.

pow(_,0) -> 1;
pow(X,1) -> X;
pow(X,Y) -> X * pow(X,Y-1).

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

Unfortunately there isnt fast implementation pow erlang function for big integers (math:pow limited by double float). So i implement
own pow function, but it super slow. Python instead have fast pow function, but python also have pow(X,Y,D) , it is super effective
implementation of pow(X,Y) % D. So python implemenation thousand times faster erlang.

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/31/project-euler-problem-26/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep26'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


