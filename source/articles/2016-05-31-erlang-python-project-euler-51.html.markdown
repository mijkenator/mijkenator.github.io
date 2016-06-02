---
title: Project Euler problem 51
date: 2016-05-31 20:16 UTC
tags: project euler, erlang, python
layout: post
---

<b>Prime digit replacements</b>

By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.


[Link to original description](https://projecteuler.net/problem=51)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p51)<br>

brutforce python solution is slow:

``` python

#!/usr/bin/python

import prime
from combinatorics import uniqueCombinations

cache = {}
def prime_family_length(n, digits):
    if cache.has_key((n, digits)): return cache[n, digits]

    num, nums, count = list(str(n)), [], 0
    if len(dict.fromkeys(num[d] for d in digits).keys()) > 1:
        return cache.setdefault((n, digits), 0)                                # The digits must have the same number

    for d in range(0 in digits and 1 or 0, 10):                                 # Ensure 0 is not the first digit
        for x in digits: num[x] = str(d)
        n = int(''.join(num))
        if prime.isprime(n): count += 1
        nums.append(n)
    for n in nums: cache[n, digits] = count
    return count

prime._refresh(100000)

n, max, max_count, combos = 10, 0, 0, {}
while max_count < 8:
    p = prime.prime(n)
    digits = range(0, len(str(p)))
    for size in xrange(1, len(digits)):
        patterns = combos.setdefault((len(digits), size),
            tuple(tuple(sorted(p)) for p in uniqueCombinations(digits, size)))
        for pat in patterns:
            count = prime_family_length(p, pat)
            if count > max_count: max, max_count = p, count
    n += 1

print p

```

Lets try to simplyfy solution algorithm. 
We are looking prime number with repeated digits. And we are looking 8 member family. And since we are looking for the smallest member of the family our repeated digits has to be 0, 1 or 2. Otherwise we wont be able to make an eight member family.

A number can be divided by 3 if sum all his digits is divisible by 3. We can use this rule to filter some prime candidates.
If we calculate the digit sum mod 3 of the repeated digits we get the following results with the number of repeated digits:

<pre>
|         | number of repeated digits   |
|         |  1  |  2  |  3  |  4  |  5  |
|_________|_____|_____|_____|_____|_____|
|   0     |  4  |  4  | 10  |  4  |  4  |
|_________|_____|_____|_____|_____|_____|
|   1     |  3  |  3  |  0  |  3  |  3  |
|_________|_____|_____|_____|_____|_____|
|   2     |  3  |  3  |  0  |  3  |  3  |
|_________|_____|_____|_____|_____|_____|

</pre>
If the number we have has 1 repeated digit then the result n % 3 will be 0 4-times -> (0,3,6,9). 
It will be 1 a total of 3 times -> (1,4,7) and 2 a total number of 3 times -> (2,5,8). This means if we have 1 repeating digit and the "digit sum mod 3" of the non repeating digits is 0 we will 
add 0 4 times. 

Which means that 4 of those numbers will be divisible with 3. 
Therefore 4 of those numbers wont be prime and we can’t have an 8-member family then. 
The same goes with all other numbers except 3, 6 and 9.

Based on that I can concluce that the repeating digit has to be three, otherwise I will never be able to make 8 different primes out of it, since too many of the candidates will be divisble by three no matter what the rest of the number is.

<b>Another explanation:</b> 

We are have number with 1 repeated digit: *xxxxx.... . If we are replace * with 1 to get a prime P. Now if P % 3 = 1 we are cannot replace * with 0,3,6,9 otherwise the number become divisible by 3 (become composite). If P % 3 = 2 we are cannot replace * with 2,5,8 for the same reason. -> making 8-family-primes are impossible for any number of digits.

We are have number with 2 repeated digits: **xxxx... . Replace both * with 1 to get prime P. If P % 3 = 2 -> cannot replace with 0,3,6,9. If P % 3 = 1 -> cannot replace with 2,5,8.

So now we know a bit more about the kind of prime we are looking for. I assume that the prime will be 6 digits. It must have 3 digit being 0,1 or 2 excluding the last digit of the number.

so we are have python version for this:

```python

#!/usr/bin/python

import prime, string
prime._refresh(80000)

def is_8_prime_family(p, d):
    c = 0
    for r in '0123456789':
        np = int(string.replace(p, d, r))
        if(np > 100000 and np < 999999 and prime.isprime(np)): c += 1
    return c==8

n=9000
while(True):
    n += 1
    p = prime.prime(n)
    if p < 100000: continue
    if p > 999999: break
    ps = str(p)
    ld = ps[5:6]
    if (ps.count('0')==3 and is_8_prime_family(ps, '0')) or (ps.count('1')==3 and ld!='1' and is_8_prime_family(ps, '1')) or \
        (ps.count('2')==3 and is_8_prime_family(ps, '2')):
        print "Answer: %s %s" % (n, ps)
        break

```

and erlang version:

``` erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p51
% vim:syn=erlang

-mode(compile).

main(_) ->
    get_primes(1000000),
    io:format("Answer ~p ~n",[ p51( lists:sort([K||{K,_}<-ets:tab2list(prim) ,K>100000,K<200000]) )]).

p51([N|T]) when N>100000 ->
    NS = integer_to_list(N),
    LD = lists:last(NS),
    case p51(NS, [LD], "0") of
        0 -> p51(T);
        A -> A 
    end;
p51([_|T]) -> p51(T).

p51(NS, "1", "1") -> p51(NS, "1", "2");
p51(NS, LD, R) ->
    case {c_count(NS,R), is_8_prime_family(NS,R), R} of
        {3, true,_} -> list_to_integer(NS);
        {_, _, "0"} -> p51(NS, LD, "1");
        {_, _, "1"} -> p51(NS, LD, "2");
        {_, _, "2"} -> 0
    end.

is_8_prime_family(Str, D) ->
    Fun = fun(N,A) ->
        case list_to_integer(re:replace(Str, D, integer_to_list(N), [global, {return, list}])) of
            NP when NP > 100000,NP < 999999 ->
                case is_prime(NP) of
                    true -> A + 1
                    ;_   -> A
                end
            ;_ -> A
        end
    end,
    Ret = lists:foldl(Fun, 0, lists:seq(0, 9)),
    Ret == 8.

c_count(Str, C) -> lists:foldl(fun(E,A) when [E]==C->A+1;(_,A)->A  end,0,Str).

%----------------------------------------------------------------------------------------------------------------------------

is_prime(N) ->
    case ets:lookup(prim, N) of
        [] -> false
        ;_ -> true
    end.
%----------------------------------------------- prime generator from Project Euler 10 (version 5 ---------------------------)
get_primes(N) ->
    ets:new(comp, [public, named_table, {write_concurrency, true} ]),
    ets:new(prim, [public, named_table, {write_concurrency, true}]),
    composite_mc(N),
    primes_mc(N),
    lists:sort([P || {P,_} <-ets:tab2list(prim)]).

primes_mc(N) ->
    case erlang:system_info(schedulers) of
        1 -> primes(N);
        C -> launch_primes(lists:seq(1,C), C, N, N div C)
    end.
launch_primes([1|T], C, N, R) -> P = self(), spawn(fun()-> primes(2,R), P ! {ok, prm} end), launch_primes(T, C, N, R);
launch_primes([H|[]], C, N, R)-> P = self(), spawn(fun()-> primes(R*(H-1)+1,N), P ! {ok, prm} end), wait_primes(C);
launch_primes([H|T], C, N, R) -> P = self(), spawn(fun()-> primes(R*(H-1)+1,R*H), P ! {ok, prm} end), launch_primes(T, C, N, R).

wait_primes(0) -> ok;
wait_primes(C) ->
    receive
        {ok, prm} -> wait_primes(C-1)
    after 1000    -> wait_primes(C)
    end.

primes(N) -> primes(2, N).
primes(I,N) when I =< N ->
    case ets:lookup(comp, I) of
        [] -> ets:insert(prim, {I,1})
        ;_ -> ok
    end,
    primes(I+1, N);
primes(I,N) when I > N -> ok.


composite_mc(N) -> composite_mc(N,2,round(math:sqrt(N)),erlang:system_info(schedulers)).
composite_mc(N,I,M,C) when I =< M, C > 0 ->
    C1 = case ets:lookup(comp, I) of
        [] -> comp_i_mc(I*I, I, N), C-1
        ;_ -> C
    end,
    composite_mc(N,I+1,M,C1);
composite_mc(_,I,M,_) when I > M -> ok;
composite_mc(N,I,M,0) ->
    receive
        {ok, cim} -> composite_mc(N,I,M,1)
    after 1000    -> composite_mc(N,I,M,0)
    end.

comp_i_mc(J, I, N) -> 
    Parent = self(),
    spawn(fun() ->
        comp_i(J, I, N),
        Parent ! {ok, cim}
    end).

comp_i(J, I, N) when J =< N -> ets:insert(comp, {J, 1}), comp_i(J+I, I, N);
comp_i(J, _, N) when J > N -> ok.


```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/05/31/erlang-python-project-euler-51/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep51'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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
