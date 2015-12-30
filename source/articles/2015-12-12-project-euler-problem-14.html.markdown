---
title: Project Euler Problem 14
date: 2015-12-12 13:32 UTC
tags: project euler, erlang, python
layout: post
---

<b>Longest Collatz sequence</b>

The following iterative sequence is defined for the set of positive integers:<br>
<pre>
n → n/2 (n is even)
n → 3n + 1 (n is odd)
</pre>

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
[Link to original description](https://projecteuler.net/problem=14)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p14)<br>

## Erlang solution
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p14
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [test14()]).

test14()                          -> test14i(2, 1, 1).
test14i(N, C, L) when N < 1000000 ->
    case lseq(N, 0) of
        Length when Length > L -> test14i(N+1, N, Length);
        _                      -> test14i(N+1, C, L)
    end;
test14i(_, C, _)                  -> C.
    
lseq(1, N)                    -> N;
lseq(P, N) when P rem 2 =:= 0 -> lseq(P div 2, N+1);
lseq(P, N)                    -> lseq(P*3 + 1, N+1).

```

## Python solution
```python
#!/usr/bin/python

cache = { 1: 1 }

def chain(cache, n):
    if not cache.get(n,0):
        if n % 2: cache[n] = 1 + chain(cache, 3*n + 1)
        else: cache[n] = 1 + chain(cache, n/2)
    return cache[n]

m,n = 0,0
for i in xrange(1, 1000000):
    c = chain(cache, i)
    if c > m: m,n = c,i

print n

```



## Performance
```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p14$ time ./p14.erl
Answer: 837799

real    0m5.928s
user    0m5.931s
sys     0m0.035s
mkh@mkh-xps:~/work/mblog/pr_euler/p14$ time ./p14.py
837799

real    0m1.600s
user    0m1.543s
sys     0m0.056s
```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/12/project-euler-problem-14/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep14'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

