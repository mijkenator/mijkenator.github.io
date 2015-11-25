---
title: Project Euler Problem 5
date: 2015-11-18 15:15 UTC
tags: project euler, erlang, perl, python
layout: post
---

<b>Smallest multiple</b>

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.<br/>
What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?<br/>
[Link to original description](https://projecteuler.net/problem=5)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p5)

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p4
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:foldl(fun(A,B)-> A*B end, 1, filter(lists:seq(2,20))),
    io:format("Answer ~p ~n",[ Answer ]).

filter([])    -> [];
filter([H|T]) -> [H | filter([ divide(X,H) || X <- T])].

divide(A, B) when A rem B =:= 0 -> A div B;
divide(A, _) -> A.

```

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p5_1
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:foldl(fun(A, B) -> A * B div gcd(A, B) end,1,lists:seq(2,20)),
    io:format("Answer ~p ~n",[ Answer ]).

gcd(A, B) when B > 0 -> gcd(B, A rem B);
gcd(A, _) -> A.

```

## Perl version
```perl
#!/usr/bin/perl
use strict;

sub gcd{
    my ($a, $b, @r) = @_;
    @r ? gcd($a, gcd($b,@r)) : $b == 0 ? $a : gcd($b, $a % $b)
}

sub lcm{
    my ($a, $b, @r) = @_;    
    @r ? lcm($a, lcm($b, @r)): $a * $b / gcd($a, $b)
}

print "Answer".lcm(1 .. 20)."\n";

```

## Python version
```python
#!/usr/bin/python

# Euclid's algorithm
def gcd(a, b): return b and gcd(b, a % b) or a 
def lcm(a, b): return a * b / gcd(a, b)

answer = 1
for i in xrange(1,21): answer = lcm(answer,i)

print "Answer %d " % answer

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '/2015/11/18/project-euler-problem-5/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep5'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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
