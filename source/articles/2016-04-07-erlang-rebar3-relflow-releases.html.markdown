---
title: rebar3 and relflow 
date: 2016-04-07 14:29 UTC
tags: erlang, rebar3, relflow, release, relup
layout: post
---

<b>Rebar3 and relflow and release upgrade and downgrade </b>

Simple how-to with examples of build, upgrade and downgrade with [rebar3](http://www.rebar3.org) and [relflow](https://github.com/RJ/relflow) plugin. 

READMORE

Lets get simple test erlang project:

```bash
mkh@mkh-xps:~/work/tmp$ git clone https://github.com/mijkenator/fifo_server.git
Cloning into 'fifo_server'...
remote: Counting objects: 30, done.
remote: Compressing objects: 100% (22/22), done.
remote: Total 30 (delta 5), reused 29 (delta 4), pack-reused 0
Unpacking objects: 100% (30/30), done.
Checking connectivity... done.
mkh@mkh-xps:~/work/tmp$ cd fifo_server
mkh@mkh-xps:~/work/tmp/fifo_server$ git log
commit 6d2b8ae21b1e64b042e9238abf41b6b74e6b8ced
Author: Mijkenator <mijkenator@gmail.com>
Date:   Wed Apr 6 15:45:05 2016 -0400

    change .gitignore file

commit 2104d2a93e8f7480c4f062ae151b354cfc077445
Author: Mijkenator <mijkenator@gmail.com>
Date:   Wed Apr 6 15:40:20 2016 -0400

    relflow added

commit d1b122037a6482bced4965b680faffde13bd37f4
Author: Mijkenator <mijkenator@gmail.com>
Date:   Wed Mar 30 09:18:34 2016 -0400

    fifo server init commit
mkh@mkh-xps:~/work/tmp/fifo_server$
```

Now checkout first commit and build initial release:

```bash
mkh@mkh-xps:~/work/tmp/fifo_server$ git checkout d1b122037a6482bced4965b680faffde13bd37f4
Note: checking out 'd1b122037a6482bced4965b680faffde13bd37f4'.

You are in 'detached HEAD' state. You can look around, make experimental
changes and commit them, and you can discard any commits you make in this
state without impacting any branches by performing another checkout.

If you want to create a new branch to retain commits you create, you may
do so (now or later) by using -b with the checkout command again. Example:

  git checkout -b new_branch_name

HEAD is now at d1b1220... fifo server init commit
```

but before we have to edit rebar.config file and change:

<pre>
{relx, [{release, { fifo_server, "0.1.0" },
</pre>

to

<pre>
{relx, [{release, { fifo_server, "first-release" },
</pre>

now we can create first release and first release tarball:

```bash
mkh@mkh-xps:~/work/tmp/fifo_server$ ./rebar3 as prod release tar
===> Verifying dependencies...
===> Fetching lager ({pkg,<<"lager">>,<<"3.0.2">>})
===> Version cached at /home/mkh/.cache/rebar3/hex/default/packages/lager-3.0.2.tar is up to date, reusing it
===> Linking _build/default/lib/lager to _build/prod/lib/lager
===> Fetching mkh_queue ({git,
                                 "https://github.com/mijkenator/mkh_queue.git",
                                 {ref,
                                  "d8e6ce951d0b8ea314dfdc77a2fd84701b6c206c"}})
===> Linking _build/default/lib/mkh_queue to _build/prod/lib/mkh_queue
===> Fetching goldrush ({pkg,<<"goldrush">>,<<"0.1.7">>})
===> Version cached at /home/mkh/.cache/rebar3/hex/default/packages/goldrush-0.1.7.tar is up to date, reusing it
===> Linking _build/default/lib/goldrush to _build/prod/lib/goldrush
===> Compiling mkh_queue
===> Compiling goldrush
===> Compiling lager
===> Compiling fifo_server
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /home/mkh/work/tmp/fifo_server/_build/prod/lib
          /home/mkh/work/tmp/fifo_server/apps
          /opt/r17.5/lib
===> Resolved fifo_server-first-release
===> Including Erts from /opt/r17.5
===> release successfully created!
===> tarball /home/mkh/work/tmp/fifo_server/_build/prod/rel/fifo_server/fifo_server-first-release.tar.gz successfully created!
mkh@mkh-xps:~/work/tmp/fifo_server$
```

and you have your first release:

```bash
mkh@mkh-xps:~/work/tmp/fifo_server$ ls -la _build/prod/rel/fifo_server/fifo_server-first-release.tar.gz 
-rw-rw-r-- 1 mkh mkh 17394355 Apr  7 11:12 _build/prod/rel/fifo_server/fifo_server-first-release.tar.gz
```

Now we can unpack and start our application on target machine or same machine in another directory:

```bash
mkh@mkh-xps:~/work/tmp/fifo_server$ mkdir ../test_server
mkh@mkh-xps:~/work/tmp/fifo_server$ cp _build/prod/rel/fifo_server/fifo_server-first-release.tar.gz ../test_server/
mkh@mkh-xps:~/work/tmp/fifo_server$ cd ../test_server/
mkh@mkh-xps:~/work/tmp/test_server$ tar xvf fifo_server-first-release.tar.gz
mkh@mkh-xps:~/work/tmp/test_server$ ls
bin  erts-6.4  fifo_server-first-release.tar.gz  lib  releases
mkh@mkh-xps:~/work/tmp/test_server$ bin/fifo_server start
mkh@mkh-xps:~/work/tmp/test_server$ bin/fifo_server attach
Attaching to /tmp/erl_pipes/fifo_server/erlang.pipe.1 (^D to exit)

(fifo_server@mkh-xps)1> application:which_applications().
[{sasl,"SASL  CXC 138 11","2.4.1"},
 {fifo_server,"FIFO TCP server","0.1.0"},
 {lager,"Erlang logging framework","3.0.2"},
 {goldrush,"Erlang event stream processor","0.1.7"},
 {syntax_tools,"Syntax tools","1.6.18"},
 {compiler,"ERTS  CXC 138 10","5.0.4"},
 {mkh_queue,"FIFO queue application","0.1.0"},
 {stdlib,"ERTS  CXC 138 10","2.4"},
 {kernel,"ERTS  CXC 138 10","3.2"}]
(fifo_server@mkh-xps)2> release_handler:which_releases().
[{"fifo_server","first-release",
  ["stdlib-2.4","kernel-3.2","mkh_queue-0.1.0",
   "compiler-5.0.4","syntax_tools-1.6.18","goldrush-0.1.7",
   "lager-3.0.2","fifo_server-0.1.0","sasl-2.4.1"],
  permanent}]
(fifo_server@mkh-xps)3>
```

So we are have installed application fifo_server and one permanent release "first-release".
Now detach from server console with Ctrl-D and back to dev directory:

<pre>
mkh@mkh-xps:~/work/tmp/test_server$ cd ../fifo_server/
mkh@mkh-xps:~/work/tmp/fifo_server$ git reset --hard origin/master
HEAD is now at 6d2b8ae change .gitignore file
mkh@mkh-xps:~/work/tmp/fifo_server$
</pre>

And we are have added relflow plogin into rebar.cofig file and added new function v/0 into apps/fifo_server/src/fifo_server_app.erl with output "1.0.0". So, we are ready to make next release. But before we have "tag" our first commit with tag "first-release" to make relflow working properly:

```bash
mkh@mkh-xps:~/work/tmp/fifo_server$ git tag -a "first-release" d1b122037a6482bced4965b680faffde13bd37f4 -m "first-release"
```

and run rebar3 relflow to make appup files:

```bash
mkh@mkh-xps:~/work/tmp/fifo_server$ ./rebar3 as prod relflow -u "first-release"
===> Replace apps/fifo_server/ebin/fifo_server.appup
===> Rewriting vsn in fifo_server.app.src
===> Rewriting release vsn in rebar.config: 20160407.174518
===> $ git add apps/fifo_server/ebin/fifo_server.appup
===> $ git add apps/fifo_server/src/fifo_server.app.src
===> $ git add rebar.config
===> $ git commit -m"relflow  --> 20160407.174518"
[detached HEAD 15bd50b] relflow  --> 20160407.174518
 3 files changed, 17 insertions(+), 19 deletions(-)
 create mode 100644 apps/fifo_server/ebin/fifo_server.appup
 rewrite apps/fifo_server/src/fifo_server.app.src (100%)

===> $ git tag -a "v20160407.174518" -m "20160407.174518"
mkh@mkh-xps:~/work/tmp/fifo_server$ 
```

with this command relflow made apps/fifo_server/src/fifo_server.app.src based on changes between commit tagged with "first-release" and current HEAD:

```bash
mkh@mkh-xps:~/work/tmp/fifo_server$ cat apps/fifo_server/ebin/fifo_server.appup
{"20160407-174518-relflow",
 [{"0.1.0",[{load_module,fifo_server_app}]}],
 [{"0.1.0",[]}]}
```

next command will create new release and release tarball:

```bash
mkh@mkh-xps:~/work/tmp/fifo_server$ ./rebar3 as prod release relup tar -u "first-release"
===> Verifying dependencies...
===> Compiling fifo_server
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /home/mkh/work/tmp/fifo_server/_build/prod/lib
          /home/mkh/work/tmp/fifo_server/apps
          /opt/r17.5/lib
          /home/mkh/work/tmp/fifo_server/_build/prod/rel
===> Resolving available OTP Releases from directories:
          /home/mkh/work/tmp/fifo_server/_build/prod/lib
          /home/mkh/work/tmp/fifo_server/_build/prod/rel
===> Resolved fifo_server-20160407.174518
===> Including Erts from /opt/r17.5
===> release successfully created!
===> relup successfully created!
===> tarball /home/mkh/work/tmp/fifo_server/_build/prod/rel/fifo_server/fifo_server-20160407.174518.tar.gz successfully created!
```

so we are have _build/prod/rel/fifo_server/fifo_server-20160407.174518.tar.gz new release, lets try to install it:

```bash
mkh@mkh-xps:~/work/tmp/fifo_server$ mkdir ../test_server/releases/20160407.174518                                                                          
mkh@mkh-xps:~/work/tmp/fifo_server$ cp _build/prod/rel/fifo_server/fifo_server-20160407.174518.tar.gz ../test_server/releases/20160407.174518/fifo_server.tar.gz
mkh@mkh-xps:~/work/tmp/fifo_server$ cd ../test_server/
mkh@mkh-xps:~/work/tmp/test_server$ bin/fifo_server install 20160407.174518
Release 20160407.174518 not found, attempting to unpack releases/20160407.174518/fifo_server.tar.gz
Unpacked successfully: "20160407.174518"
Installed Release: 20160407.174518
Made release permanent: "20160407.174518"
mkh@mkh-xps:~/work/tmp/test_server$ bin/fifo_server attach                                                                                                     
Attaching to /tmp/erl_pipes/fifo_server/erlang.pipe.1 (^D to exit)

(fifo_server@mkh-xps)3> application:which_applications().
[{sasl,"SASL  CXC 138 11","2.4.1"},
 {fifo_server,"FIFO TCP server","20160407-174518-relflow"},
 {lager,"Erlang logging framework","3.0.2"},
 {goldrush,"Erlang event stream processor","0.1.7"},
 {syntax_tools,"Syntax tools","1.6.18"},
 {compiler,"ERTS  CXC 138 10","5.0.4"},
 {mkh_queue,"FIFO queue application","0.1.0"},
 {stdlib,"ERTS  CXC 138 10","2.4"},
 {kernel,"ERTS  CXC 138 10","3.2"}]
(fifo_server@mkh-xps)4> release_handler:which_releases().
[{"fifo_server","20160407.174518",
  ["stdlib-2.4","kernel-3.2","mkh_queue-0.1.0",
   "compiler-5.0.4","syntax_tools-1.6.18","goldrush-0.1.7",
   "lager-3.0.2","fifo_server-20160407-174518-relflow",
   "sasl-2.4.1"],
  permanent},
 {"fifo_server","first-release",
  ["stdlib-2.4","kernel-3.2","mkh_queue-0.1.0",
   "compiler-5.0.4","syntax_tools-1.6.18","goldrush-0.1.7",
   "lager-3.0.2","fifo_server-0.1.0","sasl-2.4.1"],
  old}]
(fifo_server@mkh-xps)5> fifo_s
fifo_s_utils       fifo_server_app    fifo_server_sup    
(fifo_server@mkh-xps)5> fifo_server_app:v().
"1.0.0"
(fifo_server@mkh-xps)6> [Quit]
mkh@mkh-xps:~/work/tmp/test_server$
```

now we can see new release "20160407.174518" in permanent state and we are can call new function v().
So we are just did and install new release. Lets back to dev directory to make third release.
Edit apps/fifo_server/src/fifo_server_app.erl to change line v() -> "1.0.0" to v() -> "2.0.0". and commit changes.

```bash
mkh@mkh-xps:~/work/tmp/test_server$ cd ../fifo_server/
mkh@mkh-xps:~/work/tmp/fifo_server$ vim apps/fifo_server/src/fifo_server_app.erl
mkh@mkh-xps:~/work/tmp/fifo_server$ git commit -m "changes for third release" -a
mkh@mkh-xps:~/work/tmp/fifo_server$ ./rebar3 as prod relflow -u "v20160407.174518"
===> Replace apps/fifo_server/ebin/fifo_server.appup
===> Rewriting vsn in fifo_server.app.src
===> Rewriting release vsn in rebar.config: 20160407.182624
===> $ git add apps/fifo_server/ebin/fifo_server.appup
===> $ git add apps/fifo_server/src/fifo_server.app.src
===> $ git add rebar.config
===> $ git commit -m"relflow 20160407.174518 --> 20160407.182624"
[detached HEAD e396242] relflow 20160407.174518 --> 20160407.182624
 3 files changed, 5 insertions(+), 5 deletions(-)

===> $ git tag -a "v20160407.182624" -m "20160407.182624"
mkh@mkh-xps:~/work/tmp/fifo_server$ ./rebar3 as prod release relup tar -u "20160407.174518"
===> Verifying dependencies...
===> Compiling fifo_server
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          /home/mkh/work/tmp/fifo_server/_build/prod/lib
          /home/mkh/work/tmp/fifo_server/apps
          /opt/r17.5/lib
          /home/mkh/work/tmp/fifo_server/_build/prod/rel
===> Resolving available OTP Releases from directories:
          /home/mkh/work/tmp/fifo_server/_build/prod/lib
          /home/mkh/work/tmp/fifo_server/_build/prod/rel
===> Resolved fifo_server-20160407.182624
===> Including Erts from /opt/r17.5
===> release successfully created!
===> relup successfully created!
===> tarball /home/mkh/work/tmp/fifo_server/_build/prod/rel/fifo_server/fifo_server-20160407.182624.tar.gz successfully created!
mkh@mkh-xps:~/work/tmp/fifo_server$ mkdir ../test_server/releases/20160407.182624
mkh@mkh-xps:~/work/tmp/fifo_server$ cp _build/prod/rel/fifo_server/fifo_server-20160407.182624.tar.gz  ../test_server/releases/20160407.182624/fifo_server.tar.gz
mkh@mkh-xps:~/work/tmp/fifo_server$ cd ../test_server/
mkh@mkh-xps:~/work/tmp/test_server$ bin/fifo_server install 20160407.182624
Release 20160407.182624 not found, attempting to unpack releases/20160407.182624/fifo_server.tar.gz
Unpacked successfully: "20160407.182624"
Installed Release: 20160407.182624
Made release permanent: "20160407.182624"
mkh@mkh-xps:~/work/tmp/test_server$ bin/fifo_server attach                                                                                                     
Attaching to /tmp/erl_pipes/fifo_server/erlang.pipe.1 (^D to exit)

(fifo_server@mkh-xps)6> application:which_applications().
[{sasl,"SASL  CXC 138 11","2.4.1"},
 {fifo_server,"FIFO TCP server","20160407-182624-relflow"},
 {lager,"Erlang logging framework","3.0.2"},
 {goldrush,"Erlang event stream processor","0.1.7"},
 {syntax_tools,"Syntax tools","1.6.18"},
 {compiler,"ERTS  CXC 138 10","5.0.4"},
 {mkh_queue,"FIFO queue application","0.1.0"},
 {stdlib,"ERTS  CXC 138 10","2.4"},
 {kernel,"ERTS  CXC 138 10","3.2"}]
(fifo_server@mkh-xps)7> release_handler:which_releases().
[{"fifo_server","20160407.182624",
  ["stdlib-2.4","kernel-3.2","mkh_queue-0.1.0",
   "compiler-5.0.4","syntax_tools-1.6.18","goldrush-0.1.7",
   "lager-3.0.2","fifo_server-20160407-182624-relflow",
   "sasl-2.4.1"],
  permanent},
 {"fifo_server","20160407.174518",
  ["stdlib-2.4","kernel-3.2","mkh_queue-0.1.0",
   "compiler-5.0.4","syntax_tools-1.6.18","goldrush-0.1.7",
   "lager-3.0.2","fifo_server-20160407-174518-relflow",
   "sasl-2.4.1"],
  old},
 {"fifo_server","first-release",
  ["stdlib-2.4","kernel-3.2","mkh_queue-0.1.0",
   "compiler-5.0.4","syntax_tools-1.6.18","goldrush-0.1.7",
   "lager-3.0.2","fifo_server-0.1.0","sasl-2.4.1"],
  old}]
(fifo_server@mkh-xps)8>
(fifo_server@mkh-xps)8> fifo_server_app:v().             
"2.0.0"
(fifo_server@mkh-xps)9> [Quit]
mkh@mkh-xps:~/work/tmp/test_server$
```

so third release 20160407.182624  was succesfully build and installed. Lets try to downgrade to second release 20160407.174518:

```bash
mkh@mkh-xps:~/work/tmp/test_server$ bin/fifo_server downgrade 20160407.174518
Release 20160407.174518 is marked old, switching to it.
Installed Release: 20160407.174518
Made release permanent: "20160407.174518"
```



<div id="disqus_thread"></div>
<script>
/**
* RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
* LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables
*/
/*
var disqus_config = function () {
    this.page.url = '2016/04/07/erlang-rebar3-relflow-releases/'; // Replace PAGE_URL with your page's canonical URL variable
    this.page.identifier = 'er3rr'; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
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



