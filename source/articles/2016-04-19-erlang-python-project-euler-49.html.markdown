---
title: Project Euler problem 49
date: 2016-04-19 14:08 UTC
tags: project euler, erlang, python
layout: post
---

<b>Prime permutations</b>

The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

READMORE

What 12-digit number do you form by concatenating the three terms in this sequence?

[Link to original description](https://projecteuler.net/problem=49)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p49)<br>

first brutforce solution with erlang:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p49
% vim:syn=erlang

-mode(compile).

main(_) ->
    L = lists:filter(fun(1487)->false;(4817)->false;(8147)->false;(E) when E > 999 -> true;(_) -> false end, prime(10000)),
    Fn = fun(E) -> 
        M = lists:map(fun(D)  -> list_to_integer(D) end,
               lists:usort(fun(A,B) when A > B -> true;(_,_) -> false end,
                  lists:filter(fun(G)-> lists:member(list_to_integer(G), L) end,
                     permutant(integer_to_list(E))))),
        case length(M) of
            N when N > 3 -> lists:member(true, [ check_seq(IL,E) || IL <- combos(M)]);
            3            -> check_seq(M,E);
            _            -> false
        end
    end,
    Answer = lists:filter(Fn, L),
    io:format("Answer: ~p ~n", [Answer]).

check_seq([A,B,C],E) when B-A =:= C-B -> lists:member(E, [A,B,C]);
check_seq(_,_)                        -> false.
permutant([])                         -> [ []];
permutant(L)                          -> [ [H|T] || H <- L, T <- permutant(L--[H]) ].
combos(L)                             -> [ [A,B,C] || A <- L, B <- L, C <- L, A=/=B,A=/=C,B=/=C,A < B,B < C ].

prime(2)                    -> [2] ;
prime(N) when N > 2, N =< 6 -> prime(lists:seq(3,N,2),[2]);
prime(N) when N > 6         -> prime(lists:merge( [6*K-1 || K <- lists:seq(1,(N+1) div 6)], [6*K+1 || K <- lists:seq(1,(N-1) div 6)] ), [2,3]);
prime(_)                    -> [].

prime(X,P)                  -> prime(X,lists:reverse(P),P).

prime([],_,P)                                          -> lists:reverse(P) ;
prime([H|T],[HP|TP],P) when HP * HP =< H,H rem HP > 0  -> prime([H|T],TP,P);
prime([H|T],[HP|_],P)  when HP * HP > H                -> prime(T,[H|P]);
prime([_|T],_,P)                                       -> prime(T,P).

```

Now lets try to optimize algorithm. We are have to find 3 numbers: I,J,K I<J<K. And:

<pre>
J = I + N
K = J +N 

So K = J + (J - I) , because N = J - I.
</pre>

and we are have more optimal solution wuth erlang:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p49_1
% vim:syn=erlang

-mode(compile).

main(_) ->
    L = lists:filter(fun(E) when E > 1487 -> true;(_) -> false end, prime(10000)),
    io:format("Answer: ~p ~n", [get_i(L)]).

get_i([])    -> [];
get_i([I|T]) -> 
    case get_j(I, T) of
        [] -> get_i(T);
        R  -> R
    end.

get_j(_,    []) -> [];
get_j(I, [J|T]) ->
    Is = lists:sort(integer_to_list(I)),
    case lists:sort(integer_to_list(J)) == Is of
        true  -> 
            K = 2*J-I,
            case lists:sort(integer_to_list(K)) == Is of
                true ->
                    case lists:member(K, T) of
                        true -> [I,J,K] 
                        ;_   -> get_j(I, T)
                    end
                ;_ -> get_j(I, T)
            end;
        false -> get_j(I, T)
    end.


prime(2)                    -> [2] ;
prime(N) when N > 2, N =< 6 -> prime(lists:seq(3,N,2),[2]);
prime(N) when N > 6         -> prime(lists:merge( [6*K-1 || K <- lists:seq(1,(N+1) div 6)], [6*K+1 || K <- lists:seq(1,(N-1) div 6)] ), [2,3]);
prime(_)                    -> [].

prime(X,P)                  -> prime(X,lists:reverse(P),P).

prime([],_,P)                                          -> lists:reverse(P) ;
prime([H|T],[HP|TP],P) when HP * HP =< H,H rem HP > 0  -> prime([H|T],TP,P);
prime([H|T],[HP|_],P)  when HP * HP > H                -> prime(T,[H|P]);
prime([_|T],_,P)                                       -> prime(T,P).

```

and same algorithm with python:

```python
#!/usr/bin/python3

import prime

def t49():
    for n,i in enumerate(prime.prime_list):
        if i < 1488: continue
        m, ml   = n+1, len(prime.prime_list)
        ist = sorted(str(i))
        while True:
            if m >= ml: break
            j = prime.prime_list[m]
            if(sorted(str(j)) == ist):
                k = 2*j-i
                if(sorted(str(k)) == ist and prime.isprime(k)): return [i, j, k]
            m += 1
    return [0,0,0]


MAX = 10000
prime._refresh(MAX)
print("Answer: %s" % t49())

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/04/19/erlang-python-project-euler-49/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep49'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



