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
[Link to original description](https://projecteuler.net/problem=34) <br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p34)

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p34
% vim:syn=erlang

-mode(compile).

main(_) ->
    L = limit(),
    F = [fact(N)||N<-lists:seq(0,9)],
    put(fact, F),
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

### Performance
```bash
mkh@mkh-xps:~/work/mblog/pr_euler/p34$ time ./p34.pl
Answer: 40730 

real    0m9.628s
user    0m9.628s
sys     0m0.004s
mkh@mkh-xps:~/work/mblog/pr_euler/p34$ time ./p34.py
limit: 2540160
Answer: 40730

real    0m7.938s
user    0m7.854s
sys     0m0.080s
mkh@mkh-xps:~/work/mblog/pr_euler/p34$ time ./p34.escript 
Answer 40730 

real    0m7.369s
user    0m7.115s
sys     0m0.160s
mkh@mkh-xps:~/work/mblog/pr_euler/p34$
```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '/2015/11/10/project-euler-problem-34/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep34'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

