---
title: Project Euler Problem 28
date: 2016-01-13 19:52 UTC
tags: project euler, erlang, python
layout: post
---

<b>Number spiral diagonals</b>.

Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

<pre>
     43 44 45 46 47 48 49
     42 21 22 23 24 25 26
     41 20  7  8  9 10 27
     40 19  6  1  2 11 28
     39 18  5  4  3 12 29
     38 17 16 15 14 13 30
     37 36 35 34 33 32 31
</pre>

READMORE

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

Let f(n) is the function returning sum of diagonals for square with radius n (side of the square is 2n+1):
<pre>
                7 8 9
                6   2
                5 4 3
</pre>
So f(0) = 1, f(1) = 24, f(2) = 76 ......

Upper right corner is: (2n+1)², upper left is (2n+1)²-2n, bottom left is (2n+1)²-4n and the bottom right is (2n+1)²-6n
So sum will be 4(2n+1)² - 12n.

So iterative f(n) = 4(2n+1)² - 12n + f(n-1).

And we are have generator for python and recursion for erlang:

[Link to original description](https://projecteuler.net/problem=28)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p28)<br>

## Python version
```python
#!/usr/bin/python

def t28(n):
    while n > 0:
        yield 4*(2*n+1)**2 - 12*n
        n -= 1
    yield 1

print "Answer: %s" % sum(t28(500))

```

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p28
% vim:syn=erlang

-mode(compile).

main(_) -> 
    io:format("Answer ~p ~n", [t28(500)]).

t28(0) -> 1;
t28(N) -> 4*(2*N+1)*(2*N+1) - 12*N + t28(N-1).

```

Also i found interesting explanation how to build non-iterative function f(n).
Check it out [here](http://www.mathblog.dk/project-euler-28-sum-diagonals-spiral/).


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/01/13/project-euler-problem-28/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep28'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


