---
title: Project Euler Problem 22
date: 2015-12-21 18:03 UTC
tags: project euler, erlang, python
layout: post
---

<b>Names scores.</b>
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.<br>

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.<br>

What is the total of all the name scores in the file?<br>

[Link to original description](https://projecteuler.net/problem=22)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p22)<br>

## Erlang version
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p22
% vim:syn=erlang

-mode(compile).

worth(W) -> lists:foldl(fun(E,A)-> A + E - hd("A") + 1 end,0,W).

main(_) ->
    {ok, Data} = file:read_file("p022_names.txt"),
    S = size(Data)-2,
    <<"\"", D:S/binary, "\"">> = Data,
    L = lists:sort(binary:split(D,<<"\",\"">>,[global])),
    io:format("Answer: ~p ~n", [sum(L,1,0)]).

sum([], _, A)    -> A;
sum([H|T], I, A) -> sum(T,I+1,A+worth(binary_to_list(H))*I).

```

## Python version
```python
#!/usr/bin/python

def worth(name): return sum(ord(letter) - ord('A') + 1 for letter in name)

txt = open("p022_names.txt")
fl  = txt.read()[1:][:-1].split('","')
fl.sort()
 
answer = sum((i+1) * worth(fl[i]) for i in xrange(0, len(fl)))

print "Answer %s " % answer

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/21/project-euler-problem-22/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep22'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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

