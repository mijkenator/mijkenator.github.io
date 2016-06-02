---
title: Integer partition with python and erlang
date: 2016-04-29 19:25 UTC
tags: erlang, pyhon, algorithm
layout: post
---

In number theory and combinatorics, a partition of a positive integer n, also called an integer partition, is a way of writing n as a sum of positive integers. Two sums that differ only in the order of their summands are considered the same partition. 

READMORE

For example, 4 can be partitioned in four distinct ways:

<pre>
3 + 1
2 + 2
2 + 1 + 1
1 + 1 + 1 + 1
</pre>

More information you can find [here](https://en.wikipedia.org/wiki/Partition_%28number_theory%29)
and [here](http://mathworld.wolfram.com/PartitionFunctionP.html).

Recursive algorithm with python:

```python
import itertools
import uuid

#
# partitioning for integer <<n>>
#
def p0(n, permutate=False, first=True):
    answer = set()
    if not first: answer.add((n,))
    for x in range(1, n):
        for y in p0(n - x, first=False): answer.add(tuple(sorted((x,)+y)))
    if permutate:
        return sum([list(set(list(itertools.permutations(i)))) for i in answer],[])
    else:
        return answer

```

Recursive algorith with erlang:

```erlang
p0(N) -> lists:usort([lists:sort(E)||E<-fl(p0_(N,N,[]))]).
p0_(1,_, _Opts)  -> [[1]];
p0_(N,M, _Opts ) -> 
    Extra = [ [lists:sort(Y++[X])||Y<-p0_(N-X,M, _Opts)] || X<-lists:seq(1,N-1)],
    if N == M -> Extra; true -> [[N]|Extra] end.

fl([])    -> [];
fl([H|T]=L) when is_integer(H),is_list(T) ->
    case is_flat(T) of
        true  -> [lists:flatten(L)];
        false -> fl([expl(H,E) || E <- T]) 
    end;
fl([H|T]) -> fl(H) ++ fl(T);
fl(H)     -> [H].

expl(A,B) when is_list(B) -> [A|B];
expl(_,B) -> B.

is_flat(L) when is_list(L) -> length([E||E<-L, is_list(E)]) < 2;
is_flat(_) -> true. 

```

Another implementation with erlang (using process dictionary). Much faster for big numbers.

```erlang
p0_1(N)    -> p0_1_(N,N).
p0_1_(1,_) -> [[1]];
p0_1_(N,M) -> 
     Answer = make_ref(),
     if N =/= M -> put(Answer,[[N]]); true -> put(Answer, []) end,
     lists:foreach(fun(X)-> 
         lists:foreach(fun(Y)-> R = get(Answer)++[lists:sort(Y++[X])], put(Answer,R) 
                       end, p0_1_(N-X,M))
     end, lists:seq(1,N-1)),
     Ret = get(Answer), erase(Answer),
     lists:usort(Ret).

```

If we are test performance of p0 and p0_1 for N=25 with 100000 iterations:

```erlang
    {Time1,_} = timer:tc(fun(_)-> p0(N) end, [100000]),
    io:format("Time1:~p ~n", [Time1]),
    {Time2,_} = timer:tc(fun(_)-> p0_1(N) end, [100000]),
    io:format("Time2:~p ~n", [Time2]).
```
On my laptopn it will be (time in microseconds):

<pre>
Time1:184666120 
Time2:30488624
</pre>

If denominators stricted by given list, for example we can use only 1 and 2 for combinations we have to make some
changes. Python implementation:

```python
#
# partitioning for integer <<n>> with denominators from list <<ds>>
#
def p1(n, ds, permutate=False, first=True):
    answer = set()
    if not first and n in ds: answer.add((n,))
    for x in range(1, n):
        if x in ds:
            for y in p1(n - x, ds, first=False): answer.add(tuple(sorted((x,)+y)))
    if permutate:
        return sum([list(set(list(itertools.permutations(i)))) for i in answer],[])
    else:
        return answer

```

and same algorithm with erlang:

```erlang
%
% partitioning for integer <<N>> with denominators from list <<DS>>
%
p1(N,   DS) -> lists:filter(fun(L)-> lists:sum(L)==N end,lists:usort([lists:sort(E)||E<-fl(p1_(N,N,DS))])).
p1_(1,_,DS) -> case lists:member(1,DS) of true -> [[1]];_ -> [] end;
p1_(N,M,DS) ->
    Extra = [ [lists:sort(Y++[X])||Y<-p1_(N-X,M,DS), lists:member(X,DS)] || X<-lists:seq(1,N-1)],
    case {N==M, lists:member(N,DS)} of
        {false, true} -> [[N]|Extra]
        ;_ -> Extra
    end.

fl([])    -> [];
fl([H|T]=L) when is_integer(H),is_list(T) ->
    case is_flat(T) of
        true  -> [lists:flatten(L)];
        false -> fl([expl(H,E) || E <- T]) 
    end;
fl([H|T]) -> fl(H) ++ fl(T);
fl(H)     -> [H].

expl(A,B) when is_list(B) -> [A|B];
expl(_,B) -> B.

is_flat(L) when is_list(L) -> length([E||E<-L, is_list(E)]) < 2;
is_flat(_) -> true. 

```

And another limitation: if we are have limited numbers of denominators:

```python
#
# partitioning for integer <<n>> with limited number of denominators from list <<ds>>
#
def p2(n, ds, permutate=False, first=True):
    answer = set()
    if not first and n in ds: 
        answer.add((n,))
        ds.remove(n)
    for x in range(1, n):
        if x in ds:
            ds.remove(x)
            for y in p2(n - x, list(ds), first=False): answer.add(tuple(sorted((x,)+y)))
    if permutate:
        return sum([list(set(list(itertools.permutations(i)))) for i in answer],[])
    else:
        return answer

```
<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '/2016/04/29/erlang-python-partition-algorithm/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'eppa1'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



