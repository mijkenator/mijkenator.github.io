---
title: Project Euler Problem 24
date: 2015-12-28 00:43 UTC
tags: project euler, erlang, python
layout: post
---

<b>Lexicographic permutations.</b>

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:<br>
<pre>
012   021   102   120   201   210
</pre>
What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?<br>


[Link to original description](https://projecteuler.net/problem=24)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p24)<br>

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p24
% vim:syn=erlang

-mode(compile).


main(_) ->
    io:format("Answer: ~p ~n", [test24()]).

test24() ->
    L = ["0","1","2","3","4","5","6","7","8","9"],
    lists:nth(1000000,perms(L)).

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].


```

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p24
% vim:syn=erlang

-mode(compile).


main(_) ->
    io:format("Answer: ~p ~n", [test24()]).

test24() ->
    L = ["0","1","2","3","4","5","6","7","8","9"],
    lists:nth(1000000,perm(L)).

perm([]) -> [[]];
perm(L)  -> zipper(L, [], []).
 
zipper([], _, Acc)    -> lists:reverse(Acc);
zipper([H|T], R, Acc) -> prepend(H, perm(lists:reverse(R, T)), T, [H|R], Acc).
 
prepend(_, [], T, R, Acc)      -> zipper(T, R, Acc); 
prepend(X, [H|T], ZT, ZR, Acc) -> prepend(X, T, ZT, ZR, [[X|H]|Acc]).


```

## Python  version 1
```perl
#!/usr/bin/python3

def perm(n):
    a = list(range(n))
    def sub(i):
        if i == n - 1:
            yield tuple(a)
        else:
            for k in range(i, n):
                a[i], a[k] = a[k], a[i]
                yield from sub(i + 1)
            x = a[i]
            for k in range(i + 1, n):
                a[k - 1] = a[k]
            a[n - 1] = x
    yield from sub(0)

c = 0
for i in perm(10):
   c += 1
   if c == 1000000:
        print(i)
        break

```

## Python  version 2
```python
#!/usr/bin/python3
import itertools

c = 0
for values in itertools.permutations([0,1,2,3,4,5,6,7,8,9]):
    c += 1
    if c == 1000000:
        print (values)
        break

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/28/project-euler-problem-24/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep24'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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


