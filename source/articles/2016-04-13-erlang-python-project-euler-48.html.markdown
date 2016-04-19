---
title: Project Euler problem 48
date: 2016-04-13 15:58 UTC
tags: project euler, erlang, python
layout: post
---

<b>Self powers</b>

The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

READMORE

[Link to original description](https://projecteuler.net/problem=48)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p48)<br>

Python and erlang ok with big integers and simple brutforce solution working fast:

## Python version
```python
#!/usr/bin/python3

from functools import reduce

answer = reduce(lambda x,y: x+pow(y,y), range(1,1001)) % pow(10,10)

print(answer)

```

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p47
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:foldl(fun(E,A)-> pow(E,E) + A end, 0, lists:seq(1,1000)) rem pow(10,10),
    io:format("Answer: ~p ~n", [Answer]).

pow(_, 0) -> 1;
pow(A, B) -> A*pow(A, B-1).

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/04/13/erlang-python-project-euler-48/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep48'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


