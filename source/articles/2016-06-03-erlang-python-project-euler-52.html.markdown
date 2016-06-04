---
title: Project Euler problem 52
date: 2016-06-03 19:33 UTC
tags: project euler, erlang, python
layout: post
---

<b>Permuted multiples</b>

It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

READMORE

[Link to original description](https://projecteuler.net/problemi=52)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p52)<br>

Brutforce solution with one improvement: we are don't have to check all numbers. For example we are have number X, and we have
to check 2X,3X,4X,5X,6X, and all those numbers have to have same number of digits. So for any decade n we are have to check 
only first n*10 div 6 numbers.

python realization:

```python
#!/usr/bin/python3

def answer(i):
    istr = ''.join(sorted(str(i)))
    for j in range(2,7):
        if ''.join(sorted(str(i*j))) != istr: return False
    return True

n, s = 1, True;
while(s):
    n *= 10
    for i in range(n, int(n*10/6)+1):
        if answer(i):
            print("Answer %s" % i)
            s = False
            break

```

and same algorithm implemented with erlang:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p52
% vim:syn=erlang

-mode(compile).

main(_) -> io:format("Answer ~p ~n",[ p52(1) ]).

p52(N) -> 
    case answer(N*10, N*100 div 6 + 1) of
        {ok, Answer} -> Answer;
        _ -> p52(N*10)
    end.

answer(N, S) when N > S -> {error, next};
answer(N, S) ->
    NS = lists:sort(integer_to_list(N)),
    case a_(N, NS, 2) of
        true -> {ok, N}
        ;_   -> answer(N+1, S)
    end.

a_(_,_,7)    -> true;
a_(N, NS, I) ->
    case NS == lists:sort(integer_to_list(N*I)) of
        true -> a_(N, NS, I+1)
        ;_   -> false
    end.

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/06/03/erlang-python-project-euler-52/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep52'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

