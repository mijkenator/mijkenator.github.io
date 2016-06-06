---
title: Project Euler problem 53
date: 2016-06-06 17:41 UTC
tags: project euler, erlang, python
layout: post
---

<b>Combinatoric selections</b>

There are exactly ten ways of selecting three from five, 12345:

<pre>
        123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
</pre>

In combinatorics, we use the notation, C(3,5) = 10.

READMORE

In general, C(N,R) = N! / (R!*(N-R)!)

It is not until n = 23, that a value exceeds one-million: C(10,23) = 1144066.

How many, not necessarily distinct, values of  C(N,R), for 1 ≤ n ≤ 100, are greater than one-million?


[Link to original description](https://projecteuler.net/problemi=53)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p53)<br>

Brutforce python solution:

```python
import math
count = 0

for n in range(1,101):
    for r in range(1,n+1):
        if math.factorial(n) / (math.factorial(r) * math.factorial(n-r)) >= 1000000: count += 1

print("Answer %s" % count)

```

and same thing with erlang:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p53
% vim:syn=erlang

-mode(compile).

main(_) -> io:format("Answer ~p ~n",[ p53() ]).

c(N, R) -> f(N) / (f(R)*f(N-R)).

f(0) -> 1;
f(1) -> 1;
f(N) -> N*f(N-1).

p53() ->
    Fun = fun(N,A) ->
        lists:foldl(fun(R,B)->
            case c(N,R) >= 1000000 of
                true -> B + 1
                ;_   -> B
            end
        end, 0, lists:seq(1,N)) + A
    end,
    lists:foldl(Fun, 0, lists:seq(1,100)).

```

For optimization i tryed to memoize factorial function values:

for python i did it with lru_cache decorator

```python
import math, functools
count = 0

@functools.lru_cache(maxsize = 1000)
def f(n,r):
     return math.factorial(n) / (math.factorial(r) * math.factorial(n-r)) 

for n in range(1,101):
    for r in range(1,n+1):
        if f(n,r) >= 1000000: count += 1

print("Answer %s" % count)
```

for erlang i did it with process dictionary:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p53
% vim:syn=erlang

-mode(compile).

main(_) -> io:format("Answer ~p ~n",[ p53() ]).

c(N, R) -> f(N) / (f(R)*f(N-R)).

f(0) -> 1;
f(1) -> 1;
f(N) ->
    case get({'f', N}) of
        undefined -> R = N*f(N-1), put({'f', N}, R), R;
        Ret       -> Ret
    end.

p53() ->
    Fun = fun(N,A) ->
        lists:foldl(fun(R,B)->
            case c(N,R) >= 1000000 of
                true -> B + 1
                ;_   -> B
            end
        end, 0, lists:seq(1,N)) + A
    end,
    lists:foldl(Fun, 0, lists:seq(1,100)).

```

but for these calculations it is didn't make any performance increment.

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/06/06/erlang-python-project-euler-53/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep53'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


