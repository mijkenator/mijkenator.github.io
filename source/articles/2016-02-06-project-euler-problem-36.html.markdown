---
title: Project Euler Problem 36
date: 2016-02-06 22:08 UTC
tags: project euler, erlang, python
layout: post
---

<b>Double-base palindromes</b>

The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not include leading zeros.)

READMORE

[Link to original description](https://projecteuler.net/problem=35)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p35)<br>

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p36
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:sum([X||X<-lists:seq(1,999999), isDPalindrome(X)]),
    io:format("Answer ~p ~n", [ Answer ]).

isDPalindrome(X) ->
    L = integer_to_list(X),
    case L == lists:reverse(L) of
        true ->
            B = integer_to_list(X,2),
            B == lists:reverse(B)
        ;_   -> false
    end.


```

## Python version 1
```python
#!/usr/bin/python

def ispalindrome(n):
    s = str(n)
    if s == s[::-1]:
        b = bin(n)[2::]
        return b == b[::-1]
    else:
        return False

print sum(n for n in xrange(1, 1000000) if ispalindrome(n))

```

## Python version 2
```python
#!/usr/bin/python

def ispalindrome(n, base):
    digits = []
    reverse = []
    while n > 0:
        d = str(n % base)
        digits.append(d)
        reverse.insert(0, d)
        n = n / base
    return digits == reverse

print sum(n for n in xrange(1, 1000000) if ispalindrome(n, 10) and ispalindrome(n, 2))

```



<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/06/project-euler-problem-36/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep36'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

