---
title: Project Euler Problem 38
date: 2016-02-18 20:48 UTC
tags: project euler, erlang, python
layout: post
---

<b> Pandigital multiples </b>

Take the number 192 and multiply it by each of 1, 2, and 3:

<pre>
        192 × 1 = 192
        192 × 2 = 384
        192 × 3 = 576
</pre>

READMORE

By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

[Link to original description](https://projecteuler.net/problem=38)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p38)<br>

We are have to find pandigital number greater than 918273645. So first product in concatenation should start with 9.

    * N is not 9 (9  and (1,2,3,4,5) already produce 918273645)
    * N = 92 .. 98   and (1,2,3,4) cannot produce 9-digit number
    * N = 921 .. 987 and (1,2,3) too many digits
    * N = 921 .. 987 and (1,2) too few digits
    * N = 9213 .. 9876 and (1,2) -> 9-digit number
    * no other combintions works

## Erlang version
``` erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p32
% vim:syn=erlang

-mode(compile).

main(_) -> io:format("Answer ~p ~n", [t38(9876)]).

t38(9212) -> 0;
t38(N)    -> case isPandigital(integer_to_list(N) ++ integer_to_list(N*2)) of
                {true, A} -> A;
                false     -> t38(N-1)
             end.

isPandigital(N) ->
    case {length(N), lists:usort(N)} of
        {9, "123456789"} -> {true, N}
        ;_               -> false
    end.


```

## Python version
```python
#!/usr/bin/python

def t38():
    for n in range(9876,9213,-1):
        d = str(n) + str(n*2)
        if is_pandigital(d): return d
    return 0

def is_pandigital(n):
    if len(n) != 9:
        return False
    if "".join(sorted(set(n))) == "123456789":
        return True
    else:
        return False
    

print "Answer: %s" % t38() 

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/16/project-euler-problem-38/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep38'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


