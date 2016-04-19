---
title: Project Euler Problem 42
date: 2016-03-15 15:43 UTC
tags: project euler, erlang, python
layout: post
---

<b>Coded triangle numbers</b>

The n-th term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
<pre>
        1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
</pre>

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt, a 16K text file containing nearly two-thousand common English words, how many are triangle words?

READMORE

[Link to original description](https://projecteuler.net/problem=42)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p42)<br>

At first butforce python solution:


## Python version 1

```python
#!/usr/bin/python

def worth(word): return sum(ord(letter) - ord('A') + 1 for letter in word)

words = open("/home/mkh/work/mblog/pr_euler/p42/p042_words.txt").read().replace('"', '').split(',')
triangle_numbers = dict.fromkeys(list(n*(n+1)/2 for n in xrange(1, 100)), 1)

print sum(1 for word in words if worth(word) in triangle_numbers)

```

lets try to find another way without searching in generated results of sequence.
<pre>
                t = n(n-1)/2
                t = n^2/2 - n/2
                n^2 + n - 2t = 0  -- it is quadratic equation
</pre>
and it have 2 roots: (-1 +/- sqrt(1+8t))/2, but n shoukd be positive, so

<pre>
                n = (sqrt(1+8t)-1)/2
</pre>

and we have new python version:

```python
#!/usr/bin/python3

import math

words = open("/home/mkh/work/mblog/pr_euler/p42/p042_words.txt").read().replace('"', '').split(',')

def is_triangle(w):
    t = sum(ord(letter) - ord('A') + 1 for letter in w)
    n = (math.sqrt(1+8*t)-1) / 2
    return n.is_integer()

print(sum([1 for w in words if is_triangle(w)]))

```

and same with erlang:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p42
% vim:syn=erlang

is_trangle(X) ->
    T = lists:sum([C-96||C<-string:to_lower(binary_to_list(X))]),
    N = (math:sqrt(1+8*T)-1)/2, 
    case round(N) == N of
        true -> 1
        ;_   -> 0
    end.

main(_) -> 
    {ok, C} = file:read_file("/home/mkh/work/mblog/pr_euler/p42/p042_words.txt"),
    W = [binary:part(X,1,size(X)-2)||X<-binary:split(C,<<",">>,[global])],
    Answer = lists:sum([is_trangle(X)||X<-W]),
    io:format("Answer: ~p ~n", [Answer]).

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/03/15/project-euler-problem-42/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep42'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



