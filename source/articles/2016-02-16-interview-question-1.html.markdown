---
title: Coin denomimation algorithm
date: 2016-02-16 18:18 UTC
tags: interview question, erlang, python
layout: post
---

<b>Coin challenge</b>

How many different ways can N be made using any number of denominators.

For example we have denominators list: [1,3,6]. So how many ways we can made number 4 with any numbers of [1,3,6].

Answer: [1,1,1,1], [3,1], [1,3].



READMORE


## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname t1
% vim:syn=erlang

-mode(compile).

main(_) ->
    L = [1,3,6],
    N = 7,
    io:format("Answer: ~p ~n", [answer(L,N)]).

answer(L,N) -> 
    R = t31(L,N,[]),
    %clear(lists:flatten(R),0,N,[],[]).
    [lists:usort(permute(X,length(X),[]))||X<-clear(lists:flatten(R),0,N,[],[])].


t31(_, 0, A)             -> A;
t31([], _, _)            -> [];
t31(_, N, _)  when N < 0 -> [];
t31([C|Cs], N, A)        -> [t31(Cs, N, A)] ++ [t31([C|Cs], N-C, A++[C])].

clear([],_,_,[],A)   -> A;
clear([],_,_,B,A)    -> A ++ [B];
clear([H|T],N,N,B,A) -> clear([H|T],0,N,[],A++[B]);
clear([H|T],M,N,B,A) -> clear(T, M+H, N,B++[H],A).

permute(_,0,A) -> A;
permute([H|T], N, A) -> permute(T++[H], N-1, A++[T++[H]]).

```

## Python version
```python
#!/usr/bin/python

import itertools

def answer(coin, n):
    ret1 = t31(coin, n, [])
    ret2 = [[list(y) for y in list(set(list(itertools.permutations(x))))]
                for x in clear(ret1) if len(x) != 0]
    print list(clear(ret2))

def t31(coin, n, acum):
    if n == 0: return acum
    if n <  0: return []
    if len(coin) == 0: return []
    acum2 = list(acum)
    acum2.append(coin[0])
    return [t31(coin[1::], n, list(acum)), t31(coin, n-coin[0], acum2)]

def clear(lst):
    for x in lst:
        if any(isinstance(el, list) for el in x):
            for x in clear(x):
                yield x
        else: yield x
            
if __name__ == '__main__': answer([1,3,6], 7)
```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/02/16/interview-question-1/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'iq1'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

