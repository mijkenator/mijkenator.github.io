---
title: Project Euler Problem 17
date: 2015-12-16 19:51 UTC
tags: project euler, erlang, python
layout: post
---

<b>Number letter counts</b>

If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.<br>
If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?<br>

<b>NOTE:</b> Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.<br>

[Link to original description](https://projecteuler.net/problem=17)<br/>
[Source code examples on Github](https://github.com/mijkenator/pr_euler/tree/master/p17)<br>

## Erlang version 1
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p17
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [lists:foldl(fun(E,A)-> A + length(ci(E)) end,0,lists:seq(1,1000))]).

word(N) ->
    L = [
        {   0,  ""         },
        {   1,  "one"      }, 
        {   2,  "two"      }, 
        {   3,  "three"    }, 
        {   4,  "four"     }, 
        {   5,  "five"     }, 
        {   6,  "six"      }, 
        {   7,  "seven"    }, 
        {   8,  "eight"    }, 
        {   9,  "nine"     }, 
        {  10,  "ten"      }, 
        {  11,  "eleven"   }, 
        {  12,  "twelve"   }, 
        {  13,  "thirteen" }, 
        {  14,  "fourteen" }, 
        {  15,  "fifteen"  }, 
        {  16,  "sixteen"  }, 
        {  17,  "seventeen"}, 
        {  18,  "eighteen" }, 
        {  19,  "nineteen" }, 
        {  20,  "twenty"   }, 
        {  30,  "thirty"   }, 
        {  40,  "forty"    }, 
        {  50,  "fifty"    }, 
        {  60,  "sixty"    }, 
        {  70,  "seventy"  }, 
        {  80,  "eighty"   }, 
        {  90,  "ninety"   }, 
        { 100,  "hundred"  }, 
        {1000,  "thousand" } 
    ],
    proplists:get_value(N, L).

ci(1000) -> word(1) ++ word(1000);
ci(100)  -> word(1) ++ word(100);
ci(N) when N > 100 -> 
    S = ci(N div 100) ++ word(100),
    case N rem 100 of
        0 -> S;
        M -> S ++ "and" ++ ci(M)
    end;
ci(N) ->
    case word(N) of
        undefined -> ci((N div 10)*10) ++ ci(N rem 10)
        ;W        -> W
    end.
    

```
## Erlang version 2
```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname p17
% vim:syn=erlang

-mode(compile).

main(_) ->
    io:format("Answer: ~p ~n", [lists:foldl(fun(E,A)-> A + length(t17(E)) end,0,lists:seq(1,1000))]).

words() ->
        [
        {   0,  ""         },
        {   1,  "one"      }, 
        {   2,  "two"      }, 
        {   3,  "three"    }, 
        {   4,  "four"     }, 
        {   5,  "five"     }, 
        {   6,  "six"      }, 
        {   7,  "seven"    }, 
        {   8,  "eight"    }, 
        {   9,  "nine"     }, 
        {  10,  "ten"      }, 
        {  11,  "eleven"   }, 
        {  12,  "twelve"   }, 
        {  13,  "thirteen" }, 
        {  14,  "fourteen" }, 
        {  15,  "fifteen"  }, 
        {  16,  "sixteen"  }, 
        {  17,  "seventeen"}, 
        {  18,  "eighteen" }, 
        {  19,  "nineteen" }, 
        {  20,  "twenty"   }, 
        {  30,  "thirty"   }, 
        {  40,  "forty"    }, 
        {  50,  "fifty"    }, 
        {  60,  "sixty"    }, 
        {  70,  "seventy"  }, 
        {  80,  "eighty"   }, 
        {  90,  "ninety"   }, 
        { 100,  "hundred"  }, 
        {1000,  "thousand" } 
    ].

w(N) -> proplists:get_value(N, words()).

t17(1000) -> w(1) ++ w(1000);
t17(100)  -> w(1) ++ w(100);
t17(N) when N =< 20 -> w(N);
t17(N) when N < 100 -> t17i(N, lists:reverse(words()));
t17(N) when N rem 100 =:= 0 -> w(N div 100) ++ w(100);
t17(N) -> w(N div 100) ++ w(100) ++ "and" ++ t17(N rem 100). 

t17i(N, [{N, W}|_]) -> W; 
t17i(N, [{M, W}|_]) when N > M -> W ++ w(N-M);
t17i(N, [_|T]) -> t17i(N, T).

```
## Python version
```python
#!/usr/bin/python

words = [
    (   1,  'one'      , ''     ),
    (   2,  'two'      , ''     ),
    (   3,  'three'    , ''     ),
    (   4,  'four'     , ''     ),
    (   5,  'five'     , ''     ),
    (   6,  'six'      , ''     ),
    (   7,  'seven'    , ''     ),
    (   8,  'eight'    , ''     ),
    (   9,  'nine'     , ''     ),
    (  10,  'ten'      , ''     ),
    (  11,  'eleven'   , ''     ),
    (  12,  'twelve'   , ''     ),
    (  13,  'thirteen' , ''     ),
    (  14,  'fourteen' , ''     ),
    (  15,  'fifteen'  , ''     ),
    (  16,  'sixteen'  , ''     ),
    (  17,  'seventeen', ''     ),
    (  18,  'eighteen' , ''     ),
    (  19,  'nineteen' , ''     ),
    (  20,  'twenty'   , ''     ),
    (  30,  'thirty'   , ''     ),
    (  40,  'forty'    , ''     ),
    (  50,  'fifty'    , ''     ),
    (  60,  'sixty'    , ''     ),
    (  70,  'seventy'  , ''     ),
    (  80,  'eighty'   , ''     ),
    (  90,  'ninety'   , ''     ),
    ( 100,  'hundred'  , 'and'  ),
    (1000,  'thousand' , 'and'  ),
]
words.reverse()

def spell(n, words):
    word = []
    while n > 0:
        for num in words:
            if num[0] <= n:
                div = n / num[0]
                n = n % num[0]
                if num[2]: word.append(' '.join(spell(div, words)))
                word.append(num[1])
                if num[2] and n: word.append(num[2])
                break
    return word

print sum(len(word) for n in xrange(1, 1001) for word in spell(n, words))

```

<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2015/12/16/project-euler-problem-17/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep17'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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
