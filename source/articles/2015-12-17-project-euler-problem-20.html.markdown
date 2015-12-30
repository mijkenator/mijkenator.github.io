---
title: Project Euler Problem 20
date: 2015-12-17 16:18 UTC
tags: project euler, erlang, python
summary: Factorial digit sum111.
layout: post
---

<b>Factorial digit sum.</b>

n! means n × (n − 1) × ... × 3 × 2 × 1<br>
For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,<br>
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.<br>

Find the sum of the digits in the number 100!<br>

[Link to original description](https://projecteuler.net/problem=20)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p20)<br>

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p20
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [lists:foldl(fun(E,A)-> A + E - 48 end, 
                                            0, integer_to_list(fac(100)))]).

fac(1) -> 1;
fac(N) -> N*fac(N-1).

```

## Python version
```python
#!/usr/bin/python

import math

def digits(n):
    s = 0
    while n > 0:
        s = s + (n % 10)
        n = n / 10
    return s

print "Answer %s" % digits(math.factorial(100))

print "Answer (onliner) %s " % sum(map(int, str(math.factorial(100))))

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/17/project-euler-problem-20/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep20'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


