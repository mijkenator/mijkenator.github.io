---
title: Project Euler Problem 33
date: 2016-02-05 19:45 UTC
tags: project euler, erlang, python
layout: post
---

<b>Digit cancelling fractions</b>

The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

READMORE

[Link to original description](https://projecteuler.net/problem=33)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p33)<br>

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p33
% vim:syn=erlang

-mode(compile).

main(_) ->
    {N,M} = lists:foldl(fun({A,B}, {C,D})-> {A*C,B*D} end, {1,1}, 
        [{X,Y}||X<-lists:seq(10,99), Y<-lists:seq(10,99), isPhr(X,Y), X < Y]),
    io:format("Answer ~p ~n", [ round(M / N) ]).

isPhr(X,X) -> false;
isPhr(X,_) when X rem 10 == 0 -> false;
isPhr(_,Y) when Y rem 10 == 0 -> false;
isPhr(X,Y) ->
    case sets:to_list(sets:intersection(sets:from_list(integer_to_list(X)),
                                           sets:from_list(integer_to_list(Y)))) of
        []    -> false;
        Inter ->
            XX = [X1 || X1<- integer_to_list(X), lists:member(X1,Inter) == false],            
            YY = [Y1 || Y1<- integer_to_list(Y), lists:member(Y1,Inter) == false],
            case {XX,YY} of
                {"", _} -> false;
                {_, ""} -> false;
                {"0",_} -> false;
                {_,"0"} -> false;
                _ ->
                    case X/Y == list_to_integer(XX)/list_to_integer(YY) of
                        true -> true
                        ;_   -> false
                    end
            end
    end.



```

## Python version
```python
#!/usr/bin/python3

def isPhr(x, y):
    if x == y: return False
    if x % 10 == 0 and y % 10 == 0: return False

    inter = [i for i in str(y) if i in str(x)]
    if  inter != []:
       x1 = "".join([i for i in str(x) if i not in inter])
       y1 = "".join([i for i in str(y) if i not in inter])
       if x1 == "" or y1 == "": return False
       if x1 == "0" or y1 == "0": return False
       if x / y == int(x1) / int(y1):
           return True
       else:
           return False
    else:
       return False

am, bm = 1,1
for (a,b) in [(x,y) for x in range(10,100) for y in range(10,100) if x < y and isPhr(x,y)]:
        am *= a
        bm *= b 

print("Answer %s" % (bm // am))

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/05/project-euler-problem-33/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep33'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

