---
title: Project Euler Problem 16
date: 2015-12-14 17:11 UTC
tags: project euler, erlang, python
layout: post
---

<b>Power digit sum</b>

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.<br>
What is the sum of the digits of the number 2^1000?<br>

[Link to original description](https://projecteuler.net/problem=16)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p16)<br>

READMORE

## Erlang solution
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p15
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [test16(1000)]).

test16(N) ->
    lists:foldl(fun(E,A)-> A + list_to_integer([E]) end, 0, integer_to_list(round(math:pow(2,N)))).


```

##Python solution
```python
#!/usr/bin/python
def digits(n):
    s = 0
    while n > 0:
        s = s + (n % 10)
        n = n / 10
    return s

print digits(pow(2,1000))

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/14/project-euler-problem-16/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep16'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



