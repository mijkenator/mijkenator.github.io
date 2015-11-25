---
title: Project Euler problem 6
date: 2015-11-13 19:40 UTC
tags: project euler, erlang, perl, python
layout: post
---
<b>Sum square difference</b>

The sum of the squares of the first ten natural numbers is,<br/>
1^2 + 2^2 + ... + 10^2 = 385<br>
The square of the sum of the first ten natural numbers is,<br/>
(1 + 2 + ... + 10)^2 = 55^2 = 3025<br/>
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.<br/>
Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.<br/>
[Link to original description](https://projecteuler.net/problem=6)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p6)


## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p6
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = qs(),
    io:format("Answer: ~p ~n", [Answer]).

qs() -> qsi(1,{0,0}).
qsi(101,{A,B}) -> A*A - B;
qsi(N,{A,B})   -> qsi(N+1, {A+N, B + N*N}). 


```

## Perl version
```perl
#!/usr/bin/perl -w

use strict;

sub main{
    my ($a, $b) = (0, 0);
    for (1..shift){ $a+=$_; $b += $_*$_ }
    print "Answer: ".($a*$a - $b)."\n";
}

main(100);

```

## Python version
```python
#!/usr/bin/python

def main():
    r = range(1,101)
    a = sum(r)
    answer = a*a - sum(x*x for x in r)
    print "Answer: %d" % answer

main()

```


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '/2015/11/13/project-euler-problem-6/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep6'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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
