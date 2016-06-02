---
title: Integer Prime Decomposition and Prime Factorization
date: 2015-12-24 18:35 UTC
tags: erlang, pyhon, prime factorization, prime decomposition
layout: post
---

<b>Prime Factorization. </b>

Prime factorization or integer factorization of a number is the determination of the set of prime numbers which multiply together to give the original integer. It is also known as prime decomposition.<br>
See also [Integer factorization](https://en.wikipedia.org/wiki/Integer_factorization) and [Prime factorization](http://mathworld.wolfram.com/PrimeFactorization.html).

If we have function returning list of prime factors for integer N, we can produce list of the all factors for N.

Erlang version with recursion:

```erlang
%
% returning list of all factors for example:
% for 60  -> [1,2,3,4,5,6,10,12,15,20,30,60]
% for 120 -> [1,2,3,4,5,6,8,10,12,15,20,24,30,40,60,120]
%
factorization(N) ->
    F = factors(N), % get list of the prime decomposition (for 60 it will [2,2,3,5], for 120: [2,2,2,3,5])
    Elts = lists:usort(F),
    Numelts = length(Elts),
    lists:sort(gen(0, F, Elts, Numelts)).

gen(I, _, _, Numelts) when I >= Numelts -> [1];
gen(I, F, Elts, Numelts) ->
   ThisElts = lists:nth(I+1,Elts),
   ThisMax  = lists:foldl(fun(TE,A) when TE==ThisElts-> A+1;(_,A)-> A end, 0, F),
   Powers   = lists:foldl(fun(_,A)-> A ++ [lists:last(A)*ThisElts] end, [1], lists:seq(1, ThisMax)),
   lists:flatten([[PE*X||PE<-Powers] || X <- gen(I+1, F, Elts, Numelts)]).

```

Python version with generator:

```python
def all_factors(n):
   ''' Returns all factors of n, including 1 and n '''
   f = factors(n)
   elts = sorted(set(f))
   numelts = len(elts)
   def gen_inner(i):
       if i >= numelts:
           yield 1
           return
       thiselt = elts[i]
       thismax = f.count(thiselt)
       powers = [1]
       for j in xrange(thismax):
           powers.append(powers[-1] * thiselt)
       for d in gen_inner(i+1):
           for prime_power in powers:
               yield prime_power * d
   for d in gen_inner(0):
       yield d

```

Erlang version of prime factorization based on Prime generator from example 5 of [Project Euler problem 10](/2015/11/29/project-euler-problem-10/):

```erlang
factors(N) ->
    ets:new(comp, [public, named_table, {write_concurrency, true} ]),
    ets:new(prim, [public, named_table, {write_concurrency, true}]),
    NN = round(math:sqrt(N))+1,
    composite_mc(NN),
    primes_mc(NN),
    Primes = lists:sort([P||{P,_}<-ets:tab2list(prim)]),
    Fun = fun F(Num, E, A) when Num rem E =:= 0 -> F(Num div E, E, A++[E]);
              F(_, _, A) -> A
    end,
    F = lists:flatten([ Fun(N, E, []) || E <- get_primes(N, Primes, [])]),
    F ++ lists:usort(extra_primes(N, lists:usort(F))).

extra_primes(N, O) -> 
    lists:flatten([ expi(N,H,O--[H]) || H <- O ]).

expi(1, _D, _F) -> [];
expi(N,D,F) when N rem D =:= 0 -> expi(N div D, D, F);
expi(N, _,[])   -> N;
expi(N,_,[H|T]) -> expi(N, H, T).

get_primes(_, [], A)  -> A;
get_primes(N,[H,_],A) when H > N div 2 + 1 -> A;  
get_primes(N,[H|T],A) when H =< N div 2 + 1, N rem H =:= 0 -> get_primes(N, T, A ++ [H]);
get_primes(N,[_|T],A) -> get_primes(N, T, A).

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

Another version of integer factorization. It is more efficient prime version for small integers (< 1000000000):

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p23_test
% vim:syn=erlang

-mode(compile).


main([NS]) ->
    io:format("Answer: ~p ~n", [factorization(list_to_integer(NS))]).

factorization(N) -> d(N).

d(0) -> [];
d(1) -> [];
d(N) -> lists:sort(divisors(1, N)).

divisors(1, N) -> [1|divisors(2,N,math:sqrt(N))].

divisors(K,_,Q) when K > Q -> [];
divisors(K,N,_) when N rem K =/= 0 -> divisors(K+1,N,math:sqrt(N));
divisors(K,N,_) when K*K == N -> [K|divisors(K+1,N,math:sqrt(N))];
divisors(K,N,_) -> [K,N div K] ++ divisors(K+1,N,math:sqrt(N)).

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '/2015/12/24/integer_prime_decomposition_factorization/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'ipdf1'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


