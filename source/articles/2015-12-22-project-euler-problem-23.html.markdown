---
title: Project Euler Problem 23
date: 2015-12-22 21:42 UTC
tags: project euler, erlang, python
layout: post
---

<b>Non-abundant sums.</b>

A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.<br>

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.<br>

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.<br>

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.<br>

[Link to original description](https://projecteuler.net/problem=23)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p23)<br>

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p23
% vim:syn=erlang

-mode(compile).


main(_) ->
    io:format("Answer: ~p ~n", [test23()]).

test23() ->
    Fn = fun(X) ->
        case lists:sum(d(X)) of
            D when D > X -> true;
            _            -> false
        end
    end,
    Izb = lists:filter(Fn, lists:seq(1,28123)),
    ets:new(abd, [public, named_table]),
    lists:foreach(fun(E)-> ets:insert(abd,{E,1}) end, Izb),
    lists:sum(lists:filter(fun(X) -> an(X, Izb) end, lists:seq(1,28123))). 


d(0) -> [];
d(1) -> [];
d(N) -> lists:sort(divisors(1, N)).

divisors(1, N) -> [1|divisors(2,N,math:sqrt(N))].

divisors(K,_,Q) when K > Q -> [];
divisors(K,N,_) when N rem K =/= 0 -> divisors(K+1,N,math:sqrt(N));
divisors(K,N,_) when K*K == N -> [K|divisors(K+1,N,math:sqrt(N))];
divisors(K,N,_) -> [K,N div K] ++ divisors(K+1,N,math:sqrt(N)).

an(N, [H|_]) when N - H < 12 -> true;
an(N, [H|T]) ->
    case ets:lookup(abd, N-H) of
        [] -> an(N, T)
        ;_ -> false
    end.

```

## Python version
```python
#!/usr/bin/python 

import prime

MAX = 28124
#MAX = 50
prime._refresh(MAX/2)
abundants = [n for n in xrange(1, MAX) if sum(prime.all_factors(n)) > n+n]
abundants_dict = dict.fromkeys(abundants, 1)

total = 0
for n in xrange(1, MAX):
    sum_of_abundants = 0
    for a in abundants:
        if a > n: break
        if abundants_dict.get(n - a):
            sum_of_abundants = 1
            break
    if not sum_of_abundants:
        total = total + n

print total

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/22/project-euler-problem-23/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep23'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

