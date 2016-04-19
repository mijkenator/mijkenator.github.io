---
title: Project Euler Problem 43
date: 2016-03-15 17:32 UTC
tags: project euler, erlang, python
layout: post
---

<b>Sub-string divisibility</b>

The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

READMORE

<pre>
            d2d3d4=406 is divisible by 2
            d3d4d5=063 is divisible by 3
            d4d5d6=635 is divisible by 5
            d5d6d7=357 is divisible by 7
            d6d7d8=572 is divisible by 11
            d7d8d9=728 is divisible by 13
            d8d9d10=289 is divisible by 17
</pre>

Find the sum of all 0 to 9 pandigital numbers with this property.

So, lets try to simplify the problem, because brutforce take a lot of time (10! numbers will have to check).

<b>1.</b> We know d4d5d6 is divisible by 5 -> so d6 have to be 5 or 0.

<b>2.</b> and d6d7d8 is divisible by 11 and d6 = 0|5. If d6=0 -> d6d7d8=011|022|033..099 and or result will not be a pandigital. So d6 hase to be <b>5</b>.

<b>3.</b> lets find all 5XX (d6d7d8) pandigital numbers divisible by 11:

<pre>
>>> import itertools
>>> sorted([i for i in sum([[int('5'+a+b),int('5'+b+a)] for (a,b) in itertools.combinations('0123456789',2)],[]) if i%11==0])
[506, 517, 528, 539, 550, 561, 572, 583, 594]
</pre>

<b>4.</b> d7d8d9 has to be divisible by 13 and we are already know d7d8 is 06|17|28|39|50|61|72|83|94

<pre>
>>> sum([[int(i+str(n)) for n in range(0,10) if int(i+str(n)) % 13 == 0] for i in ['06','17','28','39','50','61','72','83','94']],[])
[65, 286, 390, 507, 611, 728, 832, 949]
</pre>

after exclude 611 and 949 we have 065,286,507,728,832. We have also exclude 065 and 507 because d6 is 5. So we have only 4 combinations:
<pre>
286,390,728,832
after addind d6 we have:
5286,5390,5728,5832 (d6d7d8d9)
</pre>

<b>5.</b> d8d9d10 has to be divisible by 17 and d8d9 is 86|90|28|32, so:

<pre>
>>> sum([[int(i+str(n)) for n in range(0,10) if int(i+str(n)) % 17 == 0] for i in ['86','90','28','32']],[])
[867, 901, 289, 323]
</pre>

323 has gone and we can form 3 combinations for d6d7d8d9d10:

<pre>
52867, 53901, 57289  -- it is all possible endings for our numbers
</pre>

<b>6.</b> d5d6d7 must be divisible by 7 and must end on 52, 53 or 57. ->

<pre>
>>> sum([[int(str(n)+i) for n in range(0,10) if int(str(n)+i) % 7 == 0]for i in ['52','53','57']],[])
[252, 952, 553, 357]
and we have d5d6d7d8d9d10 combinations:
952867,357289
</pre>

<b>7.</b> d2d3d4 has to be divisible by 2 -> d4 can be in 0,2,4,6,8 ->

<pre>
    d4d5d6d7d8d9d10  ->  0952867,4952867,0357289,4357289,6357289   
</pre>

<b>8.</b> d3d4d5 divisible by 3 and end with 09,49,03,43,63. And d3+d4+d5 has to be divisible by 3. So ->

<pre>
>>> sum([[int(str(n)+i) for n in range(0,10) if int(str(n)+i) % 3 == 0]for i in ['09','49','03','43','63']],[])
[9, 309, 609, 909, 249, 549, 849, 3, 303, 603, 903, 243, 543, 843, 63, 363, 663, 963]
and after excluding repeating digits we have d3d4d5d6d7d8d9d10:
30952867, 60357289, 06357289
</pre>

<b>9.</b> we have only 1 and 4 digits to add:

<pre>
>>> sum(sum([[int(i+x) for i in ['14', '41']] for x in ['30952867', '60357289', '06357289']],[]))
16695334890
</pre>

actually its it. No programm needed.


<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/03/15/project-euler-problem-43/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'pep43'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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
