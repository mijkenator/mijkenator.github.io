---
title: Project Euler problem 34
date: 2015-11-10
tags: project euler, erlang, perl, python
layout: post
---
<b>Digit factorials.</b>

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.<br/>
Find the sum of all numbers which are equal to the sum of the factorial of their digits.<br/>
Note: as 1! = 1 and 2! = 2 are not sums they are not included.<br/>
[Link to original description](https://projecteuler.net/problem=34)

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p34
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Project euler P34. ~n", []),
    L = limit(),
    io:format("hight boundary is: ~p ~n", [L]),
    F = [fact(N)||N<-lists:seq(0,9)],
    io:format("factorial precalc ~p ~n",[F]),
    put(fact, F),
    io:format("in mem: ~p ~n", [get(fact)]),
    R = calc(10,L,0),
    io:format("Answer ~p ~n",[R]),
    ok.

limit() -> fact(9)*7.

fact(0) -> 1;
fact(N) -> N*fact(N-1).

is_ok(Num) ->
    Num =:= lists:sum([lists:nth(N+1,get(fact)) || N <- list_of_digs(Num)]).


calc(N, M, A) when N =< M ->
    case is_ok(N) of
        true -> calc(N+1,M,A+N)
        ;_   -> calc(N+1,M,A)
    end;
calc(_, _, A) -> A.

list_of_digs(Num) -> lod(Num, []).
lod(0, A) -> A;
lod(N, A) -> lod(N div 10, [N rem 10]++A).

```

## Perl version

```perl
#!/usr/bin/perl -w
use strict;

my %f = ();

sub main(){
    my $limit = 7*fact(9);
    my $sum = 0;
    %f = map{ $_ => fact($_)} 0..9;
    for(my $i=10;$i<$limit;$i++){
        $sum += $i if $i == sumofd($i)    
    }
    print "Answer: $sum \n";
}

sub fact{
    my $num = shift;
    if($num == 0){ 1 }
    else{ 
        my ($f,$i) = (1, 1);
        $f *= ++$i while $i < $num;
        $f
    }
}

sub sumofd{
    my $s = 0;
    map { $s += $f{$_} } split(//, shift);
    $s
}

main();
```

## Python version
```python
#!/usr/bin/python
import math

def main():
    l = limit()
    print "limit: %d" % l
    s = 0
    f = {}
    for x in range(0, 10): f[str(x)] = math.factorial(x)
    for x in range(10, l):
        if x == sum(map(lambda y: f[y], str(x))): s += x
    print "Answer: %d" % s

def limit():
    return math.factorial(9)*7

main()
```
