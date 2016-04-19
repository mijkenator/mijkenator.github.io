---
title: Project Euler Problem 41
date: 2016-02-24 15:20 UTC
tags: project euler, erlang, python
layout: post
---

<b>Pandigital prime.</b>

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?

READMORE

[Link to original description](https://projecteuler.net/problem=41)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p41)<br>


First of all we have to try simplify problem. We have to find pandigital primes from following digits permutations:

<pre>
                            [1,2]
                            [1,2,3]
                            [1,2,3,4]
                            [1,2,3,4,5]
                            [1,2,3,4,5,6]
                            [1,2,3,4,5,6,7]
                            [1,2,3,4,5,6,7,8]
                            [1,2,3,4,5,6,7,8,9]
</pre>

[Divisible by 3 rule](https://en.wikipedia.org/wiki/Divisibility_rule#Divisibility_by_3_or_9). So rule is: 
<b>"number is divisible by 3 if and only if the digit sum of the number is divisible by 3"</b>. And we can figure out 
pandigital primes can be only with [1,2,3,4] and [1,2,3,4,5,6,7] permutations.

First of all will try to find answer from 7 digits primes:

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p41
% vim:syn=erlang

main(_) -> 
    L = lists:reverse(prime(7654321)),
    io:format("Answer: ~p ~n", [t41(L)]).

t41([H|T]) ->
    case isPandigital(integer_to_list(H)) of
        true -> H
        ;_   -> t41(T)
    end.

isPandigital(N) ->
    case {length(N), lists:usort(N)} of
        {7, "1234567"} -> true
        ;_             -> false
    end.


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

it is super slow solution, because we have to generate a lot of prime numbers befoe we can check they are pandigital:

```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p41$ time ./p41.erl
./p41.erl:24: Warning: function is_prime/1 is unused
Answer: 7652413 

real    1m45.544s
user    6m17.215s
sys     0m2.127s
```

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p41
% vim:syn=erlang

main(_) -> 
    io:format("Answer: ~p ~n", [t41(7654321)]).

t41(N) ->
    case is_prime(N) of
        true -> case isPandigital(integer_to_list(N)) of
                    true -> N
                    ;_   -> t41(N-1)
                end
        ;_   -> t41(N-1)
    end.

isPandigital(N) ->
    case {length(N), lists:usort(N)} of
        {7, "1234567"} -> true
        ;_             -> false
    end.

is_prime(N)   -> is_prime(N, 2).
is_prime(N,M) when N rem M =:= 0 -> false;
is_prime(N,M) -> 
    case (M+1) < math:sqrt(N) of
        true -> is_prime(N,M+1)
        ;_   -> true
    end.
```

this verion much faster:

```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p41$ time ./p41_1.erl
Answer: 7652413 

real    0m3.047s
user    0m3.044s
sys     0m0.016s
```

and most optimal version:

## Erlang version 3
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p41
% vim:syn=erlang

main(_) -> 
    Answer = t41([list_to_integer(string:join(X,""))||X<-permutations(["7","6","5","4","3","2","1"])]),
    io:format("Answer: ~p ~n", [Answer]).

permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])].

t41([N|T]) ->
    case is_prime(N) of
        true -> N
        ;_   -> t41(T)
    end.

is_prime(N)   -> is_prime(N, 2).
is_prime(N,M) when N rem M =:= 0 -> false;
is_prime(N,M) -> 
    case (M+1) < math:sqrt(N) of
        true -> is_prime(N,M+1)
        ;_   -> true
    end.

```
this version more 10 times faster than second version:

```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p41$ time ./p41_2.erl
Answer: 7652413 

real    0m0.252s
user    0m0.244s
sys     0m0.020s
```

And finally python implementation:

##Python version
```python
#!/usr/bin/python

import itertools, math

def is_prime(n):
    if n % 2 == 0 and n > 2: 
        return False
    for i in range(3, int(math.sqrt(n)) + 1, 2):
        if n % i == 0:
            return False
    return True

for x in list(map("".join, itertools.permutations( list("7654321") ))):
    if is_prime(int(x)):
        print "Answer: %s" % x 
        break

```

as usual python version faster than erlang

```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p41$ time ./p41.py
Answer: 7652413

real    0m0.022s
user    0m0.021s
sys     0m0.000s
```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/24/project-euler-problem-41/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep41'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



