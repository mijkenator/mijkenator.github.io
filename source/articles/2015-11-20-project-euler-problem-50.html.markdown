---
title: Project Euler Problem 50
date: 2015-11-20 16:04 UTC
tags: project euler, erlang, python
layout: post
---

<b>Consecutive prime sum</b>

The prime 41, can be written as the sum of six consecutive primes:<br/>
41 = 2 + 3 + 5 + 7 + 11 + 13<br/>
This is the longest sum of consecutive primes that adds to a prime below one-hundred.<br/>
The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.<br/>
Which prime, below one-million, can be written as the sum of the most consecutive primes?<br/>
[Link to original description](https://projecteuler.net/problem=50)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p50)


## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p50
% vim:syn=erlang

-mode(compile).

main(_) ->
    PL = prime(5000),
    Answer = calc(0,PL,PL,0,[]),
    io:format("Answer ~p ~n",[ Answer ]).

calc(_,[],_,_,A) -> fetch_results(lists:reverse(lists:usort(A)));
calc(N,[H|T], O, R, A) when R+H < 1000000 -> calc(N+1,T,O,R+H, A);
calc(N,_,O, R, A) -> L = lists:sublist(O,2,length(O)-1),calc(0,L,L,0,A ++ [{N, R}]).


fetch_results([]) -> [];
fetch_results([{N,R}|T]) ->
    case is_prime(R) of
        true -> {N,R}
        ;_   -> fetch_results(T)
    end.

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

is_prime(N)   -> is_prime(N, 2).
is_prime(N,N) -> true;
is_prime(N,M) when N rem M =:= 0 -> false;
is_prime(N,M) -> is_prime(N,M+1). 

```

## Python version
```python
prime_list = [2, 3, 5, 7, 11, 13, 17, 19, 23]   # Ensure that this is initialised with at least 1 prime
prime_dict = dict.fromkeys(prime_list, 1)
lastn      = prime_list[-1]

def _isprime(n):
    ''' Raw check to see if n is prime. Assumes that prime_list is already populated '''
    isprime = n >= 2 and 1 or 0
    for prime in prime_list:                    # Check for factors with all primes
        if prime * prime > n: break             # ... up to sqrt(n)
        if not n % prime:
            isprime = 0
            break
    if isprime: prime_dict[n] = 1               # Maintain a dictionary for fast lookup
    return isprime

def _refresh(x):
    ''' Refreshes primes upto x '''
    global lastn
    while lastn <= x:                           # Keep working until we've got up to x
        lastn = lastn + 1                       # Check the next number
        if _isprime(lastn):
            prime_list.append(lastn)            # Maintain a list for sequential access

def prime(x):
    ''' Returns the xth prime '''
    global lastn
    while len(prime_list) <= x:                 # Keep working until we've got the xth prime
        lastn = lastn + 1                       # Check the next number
        if _isprime(lastn):
            prime_list.append(lastn)            # Maintain a list for sequential access
    return prime_list[x]

def isprime(x):
    ''' Returns 1 if x is prime, 0 if not. Uses a pre-computed dictionary '''
    _refresh(x)                                 # Compute primes up to x (which is a bit wasteful)
    return prime_dict.get(x, 0)                 # Check if x is in the list

def factors(n):
    ''' Returns a prime factors of n as a list '''
    _refresh(n)
    x, xp, f = 0, prime_list[0], []
    while xp <= n:
        if not n % xp:
            f.append(xp)
            n = n / xp
        else:
            x = x + 1
            xp = prime_list[x]
    return f

def all_factors(n):
   ''' Returns all factors of n, including 1 and n '''
   f = factors(n)
   elts = sorted(set(f))
   numelts = len(elts)
   def gen_inner(i):
       if i >= numelts:
           yield 1
           return
       thiselt = elts[i]
       thismax = f.count(thiselt)
       powers = [1]
       for j in xrange(thismax):
           powers.append(powers[-1] * thiselt)
       for d in gen_inner(i+1):
           for prime_power in powers:
               yield prime_power * d
   for d in gen_inner(0):
       yield d

def num_factors(n):
    ''' Returns the number of factors of n, including 1 and n '''
    div = 1
    x = 0
    while n > 1:
        c = 1
        while not n % prime(x):
            c = c + 1
            n = n / prime(x)
        x = x + 1
        div = div * c
    return div

```


## Perl version
```perl
#!/usr/bin/perl -w

use strict;

sub primes {
    my @n = (2..shift);
    my @p;
    while (@n && (push @p, shift @n)) {
        @n = grep { $_ % $p[-1] } @n;
    }
    return @p
}


sub is_prime {
    for (2..int(sqrt($_[0]))){ return 0 unless $_[0] % $_}
    1
}

my @p = primes(5000);

my($len, $sum, $mlen, $msum) = (0, 0, 0, 0);
for my $n (0..$#p){
    for my $m ($n..$#p){
        $sum += $p[$m];
        if(is_prime($sum) && $sum < 1000000 && $mlen < $len){
            ($mlen, $msum) = ($len, $sum)
        }elsif($sum < 1000000){ $len++}
        else{ last; }
    }
    ($len, $sum) = (0,0);
}

print $mlen."--".$msum."\n";



```

## Performance 
```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p50$ time ./p50.erl
Answer {543,997651}

real    0m0.340s
user    0m0.305s
sys     0m0.078s
mkh@mkh-xps:~/work/mblog/pr_euler/p50$ time ./p50.pl
541--997651

real    0m0.897s
user    0m0.893s
sys     0m0.004s
mkh@mkh-xps:~/work/mblog/pr_euler/p50$ time ./p50.py
997651

real    0m1.377s
user    0m1.371s
sys     0m0.008s
mkh@mkh-xps:~/work/mblog/pr_euler/p50
```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '/2015/11/20/project-euler-problem-50/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep50'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

