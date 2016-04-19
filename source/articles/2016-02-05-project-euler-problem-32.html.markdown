---
title: Project Euler Problem 32
date: 2016-02-05 16:35 UTC
tags: project euler, erlang, python
layout: post
---

<b>Pandigital products</b>

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.


READMORE

[Link to original description](https://projecteuler.net/problem=32)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p32)<br>

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p32
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:sum(lists:usort([X*Y||X<-lists:seq(1,99),Y<-lists:seq(100,9999), isPandigital(X,Y)])),
    io:format("Answer ~p ~n", [Answer]).

isPandigital(X,Y) ->
    P = X * Y,
    S = integer_to_list(X) ++ integer_to_list(Y) ++ integer_to_list(P),
    case {length(S), lists:usort(S)} of
        {9, "123456789"} -> true
        ;_               -> false
    end.

```


## Python version
```python

#!/usr/bin/python

def isPandigital(x,y):
    p = x*y
    s = str(x)+str(y)+str(p)
    if len(s) != 9:
        return False
    if "".join(sorted(set(s))) == "123456789":
        return True
    else:
        return False

pd = [x*y for x in xrange(1,100) for y in xrange(100,10000) if isPandigital(x,y)]

print "Answer: %s" % sum(set(pd))
```


## Another Python version
```python
#!/usr/bin/python

from combinatorics import permutations

def num(l):
    s = 0
    for n in l: s = s * 10 + n
    return s

product = {}
for perm in permutations(range(1,10)):
    for cross in range(1,4):            # Number can't be more than 4 digits
        for eq in range(cross+1, 6):    # Result can't be less than 4 digits
            a = num(perm[0:cross])
            b = num(perm[cross:eq])
            c = num(perm[eq:9])
            if a * b == c: product[c] = 1

print sum(p for p in product)

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/32/project-euler-problem-32/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep32'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

