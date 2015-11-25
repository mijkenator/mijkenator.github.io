---
title: Project Euler problem 1
date: 2015-11-13 17:56 UTC
tags: project euler, erlang, perl, python
layout: post
---

<b>Multiples of 3 and 5</b>

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.<br/>
Find the sum of all the multiples of 3 or 5 below 1000.<br/>
[Link to original description](https://projecteuler.net/problem=1)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p1)


## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p1
% vim:syn=erlang

-mode(compile).

main(_) ->
    Answer = lists:sum([ X || X <- lists:seq(1,999),  X rem 5 =:= 0 orelse X rem 3 =:= 0]),
    io:format("Answer: ~p ~n", [Answer]).

```

## Perl version
```perl
#!/usr/bin/perl -w

use strict;

sub main(){
    my $answer = 0;
    for(1..999){$answer += $_ if(!($_ % 3) || !($_ % 5))}
    print "Answer: $answer \n"
}

main();

```

## Python version
```python
#!/usr/bin/python
import math

def main():
    answer = sum(x for x in range(1, 1000) if not(x % 3) or not(x % 5)) 
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
    this.page.url = '/2015/11/13/project-euler-problem-1/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep1'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

