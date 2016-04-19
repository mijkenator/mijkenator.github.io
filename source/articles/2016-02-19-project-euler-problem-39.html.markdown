---
title: Project Euler Problem 39
date: 2016-02-19 20:56 UTC
tags: project euler, erlang, python
layout: post
---

<b>Integer right triangles</b>

If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?

READMORE

[Link to original description](https://projecteuler.net/problem=39)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p39)<br>

## Brutforce python solution:
```python
#!/usr/bin/python

maxp, maxsol = 0, 0
for p in xrange(12, 1001, 2):
    solutions = 0
    for a in xrange(1, p/3):
        a2 = a*a
        for b in xrange(a, (p-a)/2):
            c = p - a - b
            if a2 + b*b == c*c: solutions = solutions + 1
    if solutions > maxsol: maxp, maxsol = p, solutions

print maxp

```

For right angle trinangle:

<pre>
        a^2 + b^2 = c^2
</pre>

and perimeter p = a + b + c, so c = p - a - b and:

<pre>
        a^2 + b^2 = (p-a-b)^2 = p^2 + a^2 + b^2 -2pa – 2pb + 2ab
        b = (p^2 -2pa) / (2p-2a)
</pre>

Furthermore we know a < c and b < c and without loss of generality we can assume that a ≤ b 
(otherwise we could switch them) which gives us that 
<pre>
        a ≤ b < c
</pre>
That implies  a < p/3 and thus we don’t need to check values higher than that.

## Optimized python solution
```
#!/usr/bin/python3

import operator

def get_abc(a, p):
    b = (p**2 - 2*p*a) / (2*p - 2*a)
    c = (a**2 + b**2)**0.5
    return (a, b, c)

d = dict()
for p in range(12,1001):
    d[p] = 0
    for a in range(1,p//3):
        (a, b, c) = get_abc(a, p)
        if c.is_integer(): 
            d[p] += 1

print("P: %s number:%s" % sorted(d.items(), key=operator.itemgetter(1))[-1])

```

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p39
% vim:syn=erlang

get_abc(A, P) ->
    B = (P*P - 2*P*A) / (2*P - 2*A),
    C = math:sqrt(A*A + B*B),
    case round(C) == C of
        true -> 1
        ;_   -> 0
    end.

count([], A)    -> [{P,_}|_] = lists:sort(fun({_,C},{_,D})-> C > D end, A), P;
count([H|T], A) -> count(T, [{H, proplists:get_value(H,A,0)+1}] ++ proplists:delete(H,A)).

t39() ->
    L = lists:filter(fun({_,0})-> false;(_)-> true end,
            lists:flatten([[{P,get_abc(A,P)} ||A<-lists:seq(1, P div 3)]||P<-lists:seq(12,1000)])),
    count([P||{P,_}<-L], []).


main(_) ->
    io:format("Answer: ~p ~n", [t39()]).

```

found interesting but super slow another erlang version:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p39
% vim:syn=erlang

answer() ->
    Tris = [ A+B+C || C <- lists:seq(1, 500),
                      B <- lists:seq(1, C-1),
                      A <- lists:seq(1, B),
                      (A*A + B*B) =:= (C*C),
                      (A+B+C) =< 1000 ],
    Sorted = lists:sort(Tris),
    find_p(Sorted, hd(Sorted), 0, 0, hd(Sorted)).

find_p([], _, _, _, MaxP)            -> MaxP;
find_p([P | Ps], P, C, Max, MaxP)    -> find_p(Ps, P, C+1, Max, MaxP);
find_p([P | Ps], CurP, C, Max, MaxP) ->
    case C > Max of
        true  -> find_p(Ps, P, 0, C, CurP);
        false -> find_p(Ps, P, 0, Max, MaxP)
    end.


main(_) ->
    io:format("Answer: ~p ~n", [answer()]).

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/19/project-euler-problem-39/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep39'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



