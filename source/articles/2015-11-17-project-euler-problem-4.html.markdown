---
title: Project Euler Problem 4
date: 2015-11-17 15:19 UTC
tags: project euler, erlang, perl, python
layout: post
---
<b>Largest palindrome product</b>

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.<br/>
Find the largest palindrome made from the product of two 3-digit numbers.<br/>
[Link to original description](https://projecteuler.net/problem=4)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p4)



## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p4
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:max([ X*Y || X <- lists:seq(999,100,-1), Y <- lists:seq(999,100,-1), is_palindrome(X*Y)]),
    io:format("Answer ~p ~n",[ Answer ]).


is_palindrome(N) -> S = integer_to_list(N), S =:= lists:reverse(S).

```

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p4
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:max([ p(X,X) || X <- lists:seq(999,100,-1)]),
    io:format("Answer ~p ~n",[ Answer ]).


p(X,Y)  when Y > 99  ->
    P = X*Y,
    case is_palindrome(P) of
        true -> P
        ;_   -> p(X,Y-1)
    end;
p(_,_) -> 0.

is_palindrome(N) -> S = integer_to_list(N), S =:= lists:reverse(S).

```

## Perl version
```perl
#!/usr/bin/perl -w
use strict;

my $answer = 0;
for my $x (-999..-100){
    for my $y ($x..-100){
        my $n = $x*$y;
        $answer = $n if $n > $answer && $n == reverse(split(//,$n))   
    }    
}
print "Answer: $answer \n";

```

## Python version 1
```python
#!/usr/bin/python

n = 0
for a in xrange(999, 100, -1):
     for b in xrange(a, 100, -1):
         x = a * b
         if x > n:
             s = str(x)
             if s == s[::-1]: n = x
print n

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '/2015/11/17/project-euler-problem-4/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep4'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

