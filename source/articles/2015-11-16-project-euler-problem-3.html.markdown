---
title: Project Euler Problem 3
date: 2015-11-16 19:37 UTC
tags: project euler, erlang, perl, python
layout: post
---

<b>Largest prime factor</b>

The prime factors of 13195 are 5, 7, 13 and 29.<br/>
What is the largest prime factor of the number 600851475143 ?<br/>
[Link to original description](https://projecteuler.net/problem=3)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p3)

READMORE

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p3
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer ~p ~n",[pr(1,600851475143,1)]).

pr(1, N, 1) when N rem 2 =:= 0                          -> pr(1, N, 2);
pr(1, N, A) when N rem 3 =:= 0, A < 3                   -> pr(1, N, A*3);
pr(_, N, A) when A > N                                  -> 0;
pr(K, N, A) when (N rem (6*K+1)) =:= 0, (6*K+1)*A =:= N -> 6*K+1;
pr(K, N, A) when (N rem (6*K-1)) =:= 0, (6*K-1)*A =:= N -> 6*K-1;   
pr(K, N, A)                                             ->
    case {N rem (6*K-1), N rem (6*K+1)} of
        {0, 0} -> pr(K+1, N, A*(6*K+1)*(6*K-1));
        {_, 0} -> pr(K+1, N, A*(6*K+1));
        {0, _} -> pr(K+1, N, A*(6*K-1));
        _      -> pr(K+1, N, A)
    end.


```

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p3_1
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = i(2, 600851475143),
    io:format("Answer ~p ~n",[Answer]).

i(I, N) when I*I < N -> i(I+1, j(I, N));    
i(_, N) -> N.

j(I, N) when N rem I =:= 0 -> j(I, N div I);
j(_, N) -> N.

```

## Perl version
```perl
#!/usr/bin/perl

use strict;

my ($i, $n) = (2, 600851475143);
while($i*$i < $n){
    $n /= $i while !($n % $i);
    $i++
}

print "Answer: $n \n";

```

## Python version
```python
#!/usr/bin/python

n = 600851475143
i = 2
while i * i < n:
    while n % i == 0: n = n / i
    i = i + 1

print "Answer: %d" % n

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '/2015/11/16/project-euler-problem-3/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep3'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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
