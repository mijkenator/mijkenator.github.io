---
title: Project Euler Problem 31
date: 2016-01-21 21:31 UTC
tags: project euler, erlang, python, perl
layout: post
---

<b>Coin sums</b>

In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

<pre>
        1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
</pre>

It is possible to make £2 in the following way:

<pre>
        1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
</pre>

How many different ways can £2 be made using any number of coins?

READMORE

[Link to original description](https://projecteuler.net/problem=31)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p31)<br>

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p31
% vim:syn=erlang

-mode(compile).

main(_) ->
    Coins  = [1, 2, 5, 10, 20, 50, 100, 200],
    Result = 200,  
    Answer = lists:sum(lists:flatten([f(Result, 0, subcoins(Coins, C), C) || C <- Coins])),
    io:format("Answer ~p ~n", [Answer]).

subcoins(Coins, C)                -> subcoins(Coins, C, []).
subcoins([], _, A)                -> lists:reverse(A);
subcoins([H|T], C, A) when H =< C -> subcoins(T, C, [H] ++ A);
subcoins([_|T], C, A)             -> subcoins(T, C, A).


f(Result, Ac, _, _) when Result =:= Ac -> 1;
f(Result, Ac, _, C) when Result =:= Ac + C -> 1;
f(Result, Ac, _, _) when Result <   Ac -> 0;
f(Result, Ac, Sc, C) -> [f(Result , Ac + C, subcoins(Sc, C1), C1) || C1 <- Sc].

```

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p31
% vim:syn=erlang

-mode(compile).

main(_) ->
    Coins  = [1, 2, 5, 10, 20, 50, 100, 200],
    Result = 200,  
    Answer = lists:last(t(Coins, [1] ++ [0 || _ <- lists:seq(1, Result)], Result)),
    io:format("Answer ~p ~n", [Answer]).

t([], L, _)            -> L;
t([Coin|T], L, Result) ->
   t(T, f(lists:sublist(L, 1, Coin), Coin, Coin, Result, L), Result).

f(L, E, Coin, R, L1) when E =:= R ->
    L ++ [lists:nth(E + 1, L1) + lists:nth(E - Coin + 1, L)];
f(L, E, Coin, R, L1)              ->
    f(L ++ [lists:nth(E + 1, L1) + lists:nth(E - Coin + 1, L)], E + 1, Coin, R, L1).

```

## Erlang version 3
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p31
% vim:syn=erlang

-mode(compile).

main(_) ->
    Coins  = [1, 2, 5, 10, 20, 50, 100, 200],
    Result = 200,  
    io:format("Answer ~p ~n", [t31(Coins, Result)]).

t31(_, 0)            -> 1;
t31([], _)           -> 0;
t31(_, N) when N < 0 -> 0;
t31([C|Cs], N)       -> t31(Cs, N) + t31([C|Cs], N-C).

```

## Python version
```python
#!/usr/bin/python

def test31(t, c):
    ways = [1] + [0]*t
    for coin in c:
        for i in range(coin, t+1):
            ways[i] += ways[i-coin]
    return ways[t]

coins = (1, 2, 5, 10, 20, 50, 100, 200)

print test31(200, coins)

```
## Perl version
```perl
#!/usr/bin/perl -w
use strict;

sub test31{
    my ($target, $coins) = @_;
    
    my @ways = (1) + (0) x $target;
    foreach my $coin ( @$coins ) {
        foreach my $i ( $coin .. $target ){
            $ways[$i] += $ways[$i-$coin]
        }
    }
    $ways[$target]
}

print test31(200, [1, 2, 5, 10, 20, 50, 100, 200])."\n";

```

## Performance

<pre>
mkh@mkh-xps:~/work/mblog/pr_euler/p31$ time ./p31.erl
Answer 73682 

real    0m0.765s
user    0m0.726s
sys     0m0.067s
mkh@mkh-xps:~/work/mblog/pr_euler/p31$ time ./p31_1.erl                                                                                                        
Answer 73682 

real    0m0.234s
user    0m0.220s
sys     0m0.048s
mkh@mkh-xps:~/work/mblog/pr_euler/p31$ time ./p31_2.erl                                                                                                        
Answer 73682 

real    0m0.754s
user    0m0.750s
sys     0m0.036s
mkh@mkh-xps:~/work/mblog/pr_euler/p31$ time ./p31.py
73682

real    0m0.016s
user    0m0.008s
sys     0m0.008s
mkh@mkh-xps:~/work/mblog/pr_euler/p31$ time ./p31.pl
73682

real    0m0.006s
user    0m0.003s
sys     0m0.003s
mkh@mkh-xps:~/work/mblog/pr_euler/p31$
</pre>

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/01/21/project-euler-problem-31/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep31'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



