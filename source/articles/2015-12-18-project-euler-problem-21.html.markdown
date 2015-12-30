---
title: Project Euler Problem 21
date: 2015-12-18 16:04 UTC
tags: project euler, erlang, python
layout: post
---

<b>Amicable numbers.</b>

Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).<br>
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.<br>

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.<br>

Evaluate the sum of all the amicable numbers under 10000.<br>

[Link to original description](https://projecteuler.net/problem=21)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p21)<br>

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p20
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [lists:sum(lists:flatten([fr(X) || X<-lists:seq(1,10000)]))]).

fr(X) ->
    D1 = d(X),
    case d(D1) of
        X when X < D1 -> [X, D1];
        _             -> []
    end.

d(0) -> 0;
d(N) -> lists:sum([X||X<-lists:seq(1,N div 2 + 1), N rem X =:= 0]).

```

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p20
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [lists:sum(lists:flatten([fr(X) || X<-lists:seq(1,10000)]))]).

fr(X) ->
    D1 = lists:sum(d(X, 1,[])),
    case lists:sum(d(D1,1,[])) of
        X when X < D1 -> [X, D1];
        _             -> []
    end.

d(0, _, _)                    -> [];
d(N, M, A) when M > N div 2+1 -> A;
d(N, M, A) when N rem M =:= 0 -> d(N, M+1, [M|A]);
d(N, M, A)                    -> d(N, M+1, A).

```

## Python version
```python
#!/usr/bin/python

def divisors(n): 
    return list(i for i in xrange(1, n/2+1) if n % i == 0)

pair = dict( ((n, sum(divisors(n))) for n in xrange(1, 10000)) )

print sum(n for n in xrange(1, 10000) if pair.get(pair[n], 0) == n and pair[n] != n)

```
<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/18/project-euler-problem-21/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep21'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



