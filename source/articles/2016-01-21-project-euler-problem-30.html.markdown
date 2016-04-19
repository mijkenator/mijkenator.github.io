---
title: Project Euler Problem 30
date: 2016-01-21 20:11 UTC
tags: project euler, erlang, python, perl
layout: post
---

<b>Digit fifth powers</b>.

Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

<pre>
            1634 = 1^4 + 6^4 + 3^4 + 4^4
            8208 = 8^4 + 2^4 + 0^4 + 8^4
            9474 = 9^4 + 4^4 + 7^4 + 4^4
</pre>

READMORE

As 1 = 1^4 is not a sum it is not included.<br>
The sum of these numbers is 1634 + 8208 + 9474 = 19316.<br>
Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

[Link to original description](https://projecteuler.net/problem=30)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p30)<br>

A number x with n digits and the sum s of the fifth powers of its digits satisfy x>=10^(n-1) and s<=n^5 respectively.<br> 
Thus it must be 10^(n-1)<=n^5, which implies 1<=n<=6, and x<=6*9^5.


## Python version
```python
#!/usr/bin/python

def power_of_digits(n, p):
    s = 0
    while n > 0:
        d = n % 10
        n = n / 10
        s = s + pow(d, p)
    return s


print sum(n for n in xrange(2, 6*9**5) if power_of_digits(n, 5) == n)

```

## Perl version
```perl
#!/usr/bin/perl -l

use strict;
use warnings;
use List::Util qw/sum/;

print sum grep { $_ == sum map $_**5, /./g } 2..6*9**5;

```


## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p30
% vim:syn=erlang

-mode(compile).

main(_) -> 
    Answer = lists:sum(lists:filter(fun(N)-> sump(N,10,0) == N end, lists:seq(2,6*p5(9)))),
    io:format("Answer ~p ~n", [Answer]).

p5(D) -> D*D*D*D*D.

sump(0,_,A) -> A;
sump(N,B,A) when N < B -> A+p5(N);
sump(N,B,A) -> sump(N div B, B, A + p5(N rem B)).

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/01/21/project-euler-problem-30/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep30'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


