---
title: Project Euler Problem 40
date: 2016-02-25 16:40 UTC
tags: project euler, erlang, python
layout: post
---

<b>Champernowne's constant.</b>

An irrational decimal fraction is created by concatenating the positive integers:

0.12345678910<b>1</b>112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If dn represents the nth digit of the fractional part, find the value of the following expression.

d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

READMORE

[Link to original description](https://projecteuler.net/problem=40)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p40)<br>

At first butforce python solution:

## Python version 1
```python
#!/usr/bin/python3

from functools import reduce

s = ""
for i in range(1, 1000001): s += str(i)

l = list(s)

print("Answer: %s" % reduce(lambda a,b: a*b, 
    [int(l[x-1]) for x in [1,10,100,1000,10000,100000,1000000]]))

```

it is pretty slow:

```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p40$ time ./p40.py
Answer: 210

real    0m0.503s
user    0m0.491s
sys     0m0.012s
```

So we can try to find another algorithm.
Integers 1-9 represents digits 1-9, 10-99 -> digits 10-189 (90 * 2 + 9) .... :

<pre>
        1-9: 1-9
        10-99: 10-189
        100-999: 190-2889
        1000-9999:  2890-38889
        10000-99999: 38890 – 488889
        100000-999999: 488890 – 5888889
</pre>

So, d1 = 1
d10: we are in 10-99 range -> 10-9 = 1 (we are on the first integer of the range 10-99). Each integer have 2 digits, so (10-9)/2+9 -> 9 and 1/2. This means we have first digits of tenth number. Tenth number is 10, its first digit is 1.

d100: we are also in 10-99 range -> 100 - 9 = 91 (we are on the 91 intereg of the range 10-99). As for d10: (100-9)/2+9 -> 54 and 1/2. This means the have first digits of integer 55. It is 5.

d1000: now we are in 100-999 range -> 1000-189 = 811. Each integer in this range has 3 digits: (1000-189)/3+99 = 369 and 1/3. (99 - numbers of integers from 2 previous ranges 1-9 and 10-99). So we have first digits from 370 -> 3.

and for all our numbers:

<pre>
d1 :  1/1 + 0 = 1 => 1
d10 :  (10-9)/2 + 9 = 9 1/2 => 1
d100 :  (100-9)/2 + 9 = 54 1/2 => 5
d1.000 :  (1.000-189)/3 + 99 = 369 1/3 => 3
d10.000 :  (10.000 – 2.889) / 4 + 999 = 2776 3/4 => 7
d100.000 :  (100.000 – 38.889) /5 + 9.999 = 22.221 1/5 => 2
d1.000.000 :  (1.000.000 – 488.889)/6 + 99.999  = 185.184 1/6 => 1
</pre>

and here python implementation of this algorithm:

## Python solution 2
```python
#!/usr/bin/python3
from functools import reduce

def dn(n, l):
    if n == 1: return 1
    (ln, start, end) = next((i+1,l[i-1],v) for i,v in enumerate(l) if v > n)
    d = (n - start) // ln + 10**(ln-1)
    return list(str(d))[(n-start) % ln -1]

l, m, previous = [1,10,100,1000,10000,100000,1000000], [], 0

for x in l:
    a = len(str(x))
    previous += 9*(10**(a-1))*a
    m.append(previous)


print("Answer: %s" % reduce(lambda a,b: a*b, [int(dn(x,m)) for x in l]))

```
this solution much faster brutforce solution:

```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p40$ time ./p40_1.py
Answer: 210

real    0m0.026s
user    0m0.022s
sys     0m0.004s
```

and erlang version:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p40
% vim:syn=erlang

main(_) ->
    L = [1,10,100,1000,10000,100000,1000000],
    M = gb(L, [0]),
    Answer = lists:foldl(fun(E,A)-> E*A end, 1,
        lists:map(fun(X)-> dm(X,M) end, L)),
    io:format("Answer: ~p ~n", [Answer]).

gb([], [_|A]) -> A;
gb([H|T], A)  ->
    Len = length(integer_to_list(H)),
    gb(T, A ++ [lists:last(A) + 9 * pow(10, Len-1) * Len]).

pow(X,Y)   -> pow(X,Y,1).
pow(_,0,A) -> A;
pow(X,Y,A) -> pow(X,Y-1,A*X).

dm(1, _) -> 1;
dm(N, L) ->
    {Ln, Start} = lse(N, L),
    D = (N - Start) div Ln + pow(10, Ln-1),
    list_to_integer([lists:nth((N-Start) rem Ln, integer_to_list(D))]).

lse(N,[H|T])     -> lse(N,T,H,1).
lse(N,[H|_],P,I) when H > N -> {I+1, P};
lse(N,[H|T],_,I) -> lse(N,T,H,I+1).
```

performance check:

```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p40$ time ./p40.erl
Answer: 210 

real    0m0.151s
user    0m0.136s
sys     0m0.029s
```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/25/project-euler-problem-40/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep40'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



