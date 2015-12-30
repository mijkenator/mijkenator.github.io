---
title: Project Euler Problem 15
date: 2015-12-14 16:13 UTC
tags: project euler, erlang, python
layout: post
---

<b>Lattice paths</b>
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom 
right corner.<br>
How many such routes are there through a 20×20 grid?<br>
[Link to original description](https://projecteuler.net/problem=15)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p15)<br>

[Liitle bit theory](https://en.wikipedia.org/wiki/Lattice_path)<br>

## Erlang solution 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p15
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [test15(20)]).

test15(1) -> 2;
test15(N) -> ( 4 * N -2 ) * test15(N-1) div N.


```

## Erlang solution 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p15
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [test15i(20)]).

test15i(N) -> fac(2*N) div (fac(N) * fac(N)).

fac(1) -> 1;
fac(N) -> N*fac(N-1).


```
## Python solution
```python
#!/usr/bin/python

def fact(n):
    f = 1
    for x in xrange(1, n+1): f = f * x
    return f

print("Answer: %d" % (fact(40) / fact(20) / fact(20)))

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/14/project-euler-problem-15/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep15'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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
