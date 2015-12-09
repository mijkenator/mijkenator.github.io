---
title: Project Euler Problem 10
date: 2015-11-29 21:00 UTC
tags: project euler, erlang, python
layout: post
---

<b>Summation of primes</b>

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.<br>
Find the sum of all the primes below two million.<br>
[Link to original description](https://projecteuler.net/problem=10)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p10)

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p10
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = sum(2000000,2,0),
    io:format("Answer: ~p ~n", [Answer]).

sum(Max, Start, Sum) when Start =< Max ->
    case isprime(Start, 2, math:sqrt(Start)) of
        true -> sum(Max, Start+1, Sum + Start);
        _    -> sum(Max, Start+1,Sum)
    end;
sum(_, _, Sum) -> Sum.

isprime(N,PFactor,Max) when PFactor =< Max, N rem PFactor =:= 0 -> false;
isprime(N,PFactor,Max) when PFactor =< Max                      -> isprime(N,PFactor+1,Max);
isprime(_, _, _)                                                -> true.

```
<b>performance test:</b><br/>

```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p10$ time ./p10.erl
Answer: 142913828922 

real    0m8.845s
user    0m8.837s
sys     0m0.042s

```

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p10_1
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:foldl(fun(E,A)-> E+A end, 0, sieve(2000000)),
    io:format("Answer: ~p ~n", [Answer]).

sieve(N) -> [1] ++ sieve(lists:seq(2, N), N).

sieve([Head|L], N) when Head * 2 < N ->
    [Head] ++ sieve(L -- lists:seq(Head * 2, N, Head), N);
sieve([Head|L], _) -> [Head] ++ L.

```

<b>performance test:</b><br/>

```bash

mkh@mkh-xps:~/work/mblog/pr_euler/p10$ time ./p10_1.erl
^C

real    1m28.705s
user    1m28.664s
sys     0m0.088s

```
so, performance test is failed, i stopped script after 1 and half minute. This implementation working good
for small number, but really slow.

## Erlang version 3
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p10_2
% vim:syn=erlang

-mode(compile).

sieve(Candidates,SearchList,Primes,_Maximum) when length(SearchList) == 0 ->
    ordsets:union(Primes,Candidates);
sieve(Candidates,SearchList,Primes,Maximum)  ->
     H = lists:nth(1,string:substr(Candidates,1,1)),
     Reduced1 = ordsets:del_element(H, Candidates),
     {Reduced2, ReducedSearch} = remove_multiples_of(H, Reduced1, SearchList),
     NewPrimes = ordsets:add_element(H,Primes),
     sieve(Reduced2, ReducedSearch, NewPrimes, Maximum).
 
remove_multiples_of(Number,Candidates,SearchList) ->                                 
    NewSearchList = ordsets:filter( fun(X) -> X >= Number * Number end, SearchList), 
    RemoveList = ordsets:filter( fun(X) -> X rem Number == 0 end, NewSearchList),
    {ordsets:subtract(Candidates, RemoveList), ordsets:subtract(NewSearchList, RemoveList)}.
 
main(_) ->      
    N = 2000000,
    CandidateList = lists:seq(3,N,2),
    Candidates = ordsets:from_list(CandidateList),
    ResultSet = ordsets:add_element(2,sieve(Candidates,Candidates,ordsets:new(),N)),
    io:format("Answer: ~p ~n", [lists:foldl(fun(E,A)->E+A end, 0, ResultSet)]).

```
this implementation i found on [Rosettacode.org](http://rosettacode.org/wiki/Sieve_of_Eratosthenes#Erlang) and decide to try it

<b>performance test:</b><br/>

```bash

mkh@mkh-xps:~/work/mblog/pr_euler/p10$ time ./p10_2.erl
Answer: 142913828922 

real    0m5.608s
user    0m5.571s
sys     0m0.083s

```
it was better but i decide to try to use ets tables instead dict, see next example

## Erlang version 4
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p10_3
% vim:syn=erlang
 
-mode(compile).

main(_) ->
    N = 2000000,
    ets:new(comp, [public, named_table]),
    ets:new(prim, [public, named_table]),
    composite(N),
    primes(N),
    Answer = lists:foldl(fun({E,_},A)-> E+A end, 0, ets:tab2list(prim)),
    io:format("Answer: ~p ~n", [Answer]).

primes(N) -> primes(2, N).
primes(I,N) when I =< N ->
    case ets:lookup(comp, I) of
        [] -> ets:insert(prim, {I,1})
        ;_ -> ok
    end,
    primes(I+1, N);
primes(I,N) when I > N -> ok.

composite(N) -> composite(N,2,round(math:sqrt(N))).
composite(N,I,M) when I =< M ->
    case ets:lookup(comp, I) of
        [] -> comp_i(I*I, I, N)
        ;_ -> ok
    end,
    composite(N,I+1,M);
composite(_,_,_) -> ok.

comp_i(J, I, N) when J =< N -> ets:insert(comp, {J, 1}), comp_i(J+I, I, N);
comp_i(J, _, N) when J > N -> ok.

```
it have pretty same preformance:

```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p10$ time ./p10_3.erl
Answer: 142913828922 

real    0m5.543s
user    0m5.501s
sys     0m0.068s

```

and after that i decide to let this implementation to use all CPU cores, see next example.
This script getting number of available CPU cores by erlang:system_info(schedulers) and spread calculation between them.

## Erlang version 5. Sieve of Eratosthenes, simultaneous, multi-core implementation 
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p10_4
% vim:syn=erlang
 
-mode(compile).

main(_) ->
    N = 20000000,
    ets:new(comp, [public, named_table, {write_concurrency, true} ]),
    ets:new(prim, [public, named_table, {write_concurrency, true}]),
    composite_mc(N),
    io:format("com ok ~n", []),
    primes_mc(N),
    Answer = lists:foldl(fun({E,_},A)-> E+A end, 0, ets:tab2list(prim)),
    io:format("Answer: ~p ~n", [Answer]).

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
<b>performance</b> <br>

```bash

mkh@mkh-xps:~/work/mblog/pr_euler/p10$ time ./p10_4.erl
Answer: 142913828922 

real    0m2.436s
user    0m8.029s
sys     0m0.135s

```

much better. And i test 3,4,5 scripts for 20 000 000 primes.

```bash

mkh@mkh-xps:~/work/mblog/pr_euler/p10$ time ./p10_2.erl  
Answer: 12272577818052 

real    1m59.066s
user    1m56.761s
sys     0m2.359s

mkh@mkh-xps:~/work/mblog/pr_euler/p10$ time ./p10_3.erl
Answer: 12272577818052 

real    1m6.099s
user    1m5.950s
sys     0m0.213s

mkh@mkh-xps:~/work/mblog/pr_euler/p10$ time ./p10_4.erl     
Answer: 12272577818052 

real    0m29.205s
user    1m43.910s
sys     0m1.373s

```

so, multicore ets version won.
But python version almost 10 times faster :)

## Python version
```python
#!/usr/bin/python

sieve = [True] * 2000000 

def mark(sieve, x):
    for i in xrange(x+x, len(sieve), x):
        sieve[i] = False

for x in xrange(2, int(len(sieve) ** 0.5) + 1):
    if sieve[x]: mark(sieve, x)

print sum(i for i in xrange(2, len(sieve)) if sieve[i])

```

<b>performance test:</b>

```bash

mkh@mkh-xps:~/work/mblog/pr_euler/p10$ time ./p10.py
142913828922

real    0m0.354s
user    0m0.351s
sys     0m0.004s

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/11/29/project-euler-problem-10/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep10'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

