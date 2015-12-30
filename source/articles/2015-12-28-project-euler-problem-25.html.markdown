---
title: Project Euler Problem 25
date: 2015-12-28 20:51 UTC
tags: project euler, erlang, python
layout: post
---

<b>1000-digit Fibonacci number.</b>

The Fibonacci sequence is defined by the recurrence relation:<br>
F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.<br>

READMORE

Hence the first 12 terms will be:<br>
<pre>
F(1) = 1
F(2) = 1
F(3) = 2
F(4) = 3
F(5) = 5
F(6) = 8
F(7) = 13
F(8) = 21
F(9) = 34
F(10) = 55
F(11) = 89
F(12) = 144
</pre>

The 12th term, F12, is the first term to contain three digits.<br>
What is the index of the first term in the Fibonacci sequence to contain 1000 digits?<br>

[Link to original description](https://projecteuler.net/problem=25)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p25)<br>

burtforce solution:

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p25
% vim:syn=erlang

-mode(compile).


main(_) ->
    io:format("Answer: ~p ~n", [test25()]).

test25()     -> fib(1,2,2).
fib(A,B,N)   ->
    case length(integer_to_list(A)) of
        1000 -> N;
        _    -> fib(B, A+B,N+1)
    end.

```

after that i found this [Golden Ratio](https://en.wikipedia.org/wiki/Golden_ratio) and this: [Fibonacci number](https://en.wikipedia.org/wiki/Fibonacci_number). So more effective solution:

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p25
% vim:syn=erlang

-mode(compile).


main(_) ->
    io:format("Answer: ~p ~n", [test25()]).

test25() -> 
    Phi    = (math:sqrt(5) + 1) / 2,
    C      = math:log10(5) / 2,
    LogPhi = math:log10(Phi),
    t25i(1, C, LogPhi).

t25i(N, C, L) when N * L - C >= 999 -> N;
t25i(N, C, L) -> t25i(N+1, C, L).

```

## Python version
```python
#!/usr/bin/python3
import math

phi = (1 + pow(5, 0.5)) / 2
c = math.log10(5) / 2
logphi = math.log10(phi)
n = 1
while True:
    if n * logphi - c >= 999:
        break
    n = n + 1

print("Answer %s " % n)

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/28/project-euler-problem-25/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep25'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



