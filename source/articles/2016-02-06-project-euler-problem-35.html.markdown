---
title: Project Euler Problem 35
date: 2016-02-06 20:29 UTC
tags: project euler, erlang, python
layout: post
---

<b>Circular primes</b>

The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100:

<pre>
2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
</pre>

How many circular primes are there below one million?

READMORE


[Link to original description](https://projecteuler.net/problem=35)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p35)<br>

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p35
% vim:syn=erlang

-mode(compile).

main(_) ->
    L = [P||P<-prime(1000000), cirlcePrime(P)],
    io:format("Answer ~p ~n", [ length(L)]).

cirlcePrime(P) ->
    PL = integer_to_list(P),
    L  = permute(PL,length(PL),[]) -- PL,
    L1 = length(L),
    L2 = length([X||X<-L, is_prime(list_to_integer(X))]),
    L1 == L2.

is_prime(X) ->
    case ets:lookup(prim, X) of
        [] -> false
        ;_ -> true
    end.

permute(_,0,A) -> A;
permute([H|T], N, A) -> permute(T++[H], N-1, A++[T++[H]]).

%------------------------------------- prime library from problem 10------------

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

def mark(sieve, x):
    for i in xrange(x+x, len(sieve), x):
        sieve[i] = False

def circular(n):
    digits = []
    while n > 0:
        digits.insert(0, str(n % 10))
        n = n / 10
    for d in xrange(1, len(digits)):
        yield int(''.join(digits[d:] + digits[0:d]))

sieve = [True] * 1000000
sieve[0] = sieve[1] = False

for x in xrange(2, int(len(sieve) ** 0.5) + 1):
    mark(sieve, x)

count = 0
for n, p in enumerate(sieve):
    if p:
        iscircularprime = 1
        for m in circular(n):
            if not sieve[m]:
                iscircularprime = 0
                break
        if iscircularprime:
            count = count + 1

print count

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/06/project-euler-problem-35/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep35'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


