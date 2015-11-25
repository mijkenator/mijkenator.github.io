---
title: Project Euler Problem 7
date: 2015-11-22 02:12 UTC
tags: project euler, erlang, python
layout: post
---

<b>10001st prime</b>

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.<br/>
What is the 10 001st prime number?<br/>
[Link to original description](https://projecteuler.net/problem=7)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p7)


## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p7
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:nth(10001, prime(110000)),
    io:format("Answer: ~p ~n", [Answer]).


prime(2)                    -> [2] ;
prime(N) when N > 2, N =< 6 -> prime(lists:seq(3,N,2),[2]);
prime(N) when N > 6         ->
    prime(lists:merge( [6*K-1 || K <- lists:seq(1,(N+1) div 6)], [6*K+1 || K <- lists:seq(1,(N-1) div 6)] ), [2,3]);
prime(_)                    -> [].

prime(X,P)                  -> prime(X,lists:reverse(P),P).

prime([],_,P)                                          -> lists:reverse(P) ;
prime([H|T],[HP|TP],P) when HP * HP =< H,H rem HP > 0  -> prime([H|T],TP,P);
prime([H|T],[HP|_],P)  when HP * HP > H                -> prime(T,[H|P]);
prime([_|T],_,P)                                       -> prime(T,P).

```

## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p7_1
% vim:syn=erlang

-mode(compile).

main(_) ->
    N = 10001,
    Answer = lists:nth(N,erato(lists:seq(2,110000), 1, N)),
    io:format("Answer: ~p ~n", [Answer]).


erato(L, C, C)     -> L;
erato([H|T], N, C) -> [H| erato([ X || X <- T, X rem H =/= 0],N+1, C)].

```

## Performance
```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p7$ time ./p7.erl
Answer: 104743

real    0m1.409s
user    0m1.390s
sys     0m0.063s
mkh@mkh-xps:~/work/mblog/pr_euler/p7$ time ./p7_1.erl
Answer: 104743

real    0m1.980s
user    0m1.957s
sys     0m0.085
```

## Perl version
```
#!/usr/bin/perl -w
use strict;

sub primes {
    my @n = (2..110000);
    my @p;
    while (@n && (push @p, shift @n) && $#p < 10000) {
        @n = grep { $_ % $p[-1] } @n;
    }
    return pop @p
}

print "Answer:".primes()."\n";

```

## Python version
```
#!/usr/bin/python

import prime
print prime.prime(10000)

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '/2015/11/22/project-euler-problem-7/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep7'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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
