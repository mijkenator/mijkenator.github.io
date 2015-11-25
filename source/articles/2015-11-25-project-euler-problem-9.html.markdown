---
title: Project Euler Problem 9
date: 2015-11-25 18:22 UTC
tags: project euler, erlang, python
layout: post
---

<b>Special Pythagorean triplet</b>

A Pythagorean triplet is a set of three natural numbers, a b c, for which,<br>

a^2 + b^2 = c^2<br>
For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.<br>

There exists exactly one Pythagorean triplet for which a + b + c = 1000.<br>
Find the product abc.<br>
[Link to original description](https://projecteuler.net/problem=9)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p9)


## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p9
% vim:syn=erlang

-mode(compile).

main(_) ->
    D = 1000,
    Answer = lists:last([ X*Y*(D-X-Y) || X <- lists:seq(1, D), Y <- lists:seq(1, D), X*X + Y*Y =:= (D-X-Y)*(D-X-Y)]),
    io:format("Answer: ~p ~n", [Answer]).

```
## Erlang version (much efficient)
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p9_1
% vim:syn=erlang

-mode(compile).
-define(N, 1000).

main(_) ->
    io:format("Answer: ~p ~n", [a(1)]).

a(X) when X < ?N ->
    case b(X,X) of
        false -> a(X+1);
        Ret   -> Ret
    end.

b(X,Y) when Y < ?N, X*X+Y*Y =:= (?N-X-Y)*(?N-X-Y) -> X*Y*(?N-X-Y);
b(X,Y) when Y < ?N -> b(X,Y+1);
b(_,_) -> false.

```

## Perl version
```perl
#!/usr/bin/perl -w
use strict;

for my $a (1..1000){
    for my $b ($a..1000){
        my $c = 1000 - $a - $b;
        if($c > 0 && ($a*$a+$b*$b == $c*$c)){
            print "Answer: ".$a*$b*$c."\n";
            last;
        }
    }    
}

```

## Python version
```python
#!/usr/bin/python

for a in xrange(1, 1000):
    for b in xrange(a, 1000):
        c = 1000 - a - b
        if c > 0:
            if c*c == a*a + b*b:
                answer = a*b*c
                print "Answer %d " % answer
                break

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/11/25/project-euler-problem-9/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep9'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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
