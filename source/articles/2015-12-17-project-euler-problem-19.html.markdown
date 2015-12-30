---
title: Project Euler Problem 19
date: 2015-12-17 16:17 UTC
tags: project euler, erlang, python
layout: post
---

<b>Counting Sundays.</b>

You are given the following information, but you may prefer to do some research for yourself.<br>
<ul>
    <li>1 Jan 1900 was a Monday.</li>
    <li>Thirty days has September, April, June and November.</li>
    <li>All the rest have thirty-one.</li>
    <li>Saving February alone, Which has twenty-eight, rain or shine.</li>
    <li>And on leap years, twenty-nine.</li>
</ul>

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?<br>

[Link to original description](https://projecteuler.net/problem=19)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p19)<br>

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p19
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [length([ 1 || Y <- lists:seq(1901, 2000),  
                                              M <- lists:seq(1, 12), 
                                              calendar:day_of_the_week(Y,M,1) =:= 7 ])]).

```

## Python version
```python
#!/usr/bin/python

import datetime

sundays = 0
for year in xrange(1901, 2001):
    for month in xrange(1, 13):
        d = datetime.date(year, month, 1)
        if d.weekday() == 6:
            sundays = sundays + 1

print sundays

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/17/project-euler-problem-19/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep19'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

