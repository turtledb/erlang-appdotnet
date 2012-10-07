

#App.net for Erlang#


Copyright (c) 2012 Erik Hedenstr&ouml;m

__Version:__ 1.0.0

__Authors:__ Erik Hedenstr&ouml;m ([`erik@hedenstroem.com`](mailto:erik@hedenstroem.com)).

__License:__ MIT

__Build Status:__ [![Build Status](https://secure.travis-ci.org/ehedenst/erlang-appdotnet.png)](http://travis-ci.org/ehedenst/erlang-appdotnet)

###<a name="Donations_&amp;_Endorsements">Donations & Endorsements</a>##


Any donation is much appreciated! It will be used to pay for my [app.net](https://alpha.app.net/erikh) developer account as well as maintaining this project.


[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=ehedenst&url=https://github.com/ehedenst/erlang-appdotnet&title=App.net%20for%20Erlang&language=&tags=github&category=software)


[![Endorse ehedenst on Coderwall](http://api.coderwall.com/ehedenst/endorsecount.png)](http://coderwall.com/ehedenst)

###<a name="Quick_Start">Quick Start</a>##


Replace "..." in the examples below with an access token from [your apps](https://alpha.app.net/developer/apps/).

<pre>
    AccessToken = "...",
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ibrowse),
    {ok, Data} = appdotnet:retrieve_user(AccessToken, "@erikh").
</pre>

or

<pre>
    AccessToken = "...",
    application:start(appdotnet),
    {ok, Pid} = appdotnet_client:start(),
    {ok, Data} = appdotnet:retrieve_user(Pid, AccessToken, "@erikh").
</pre>


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="anonymous_tests.md" class="module">anonymous_tests</a></td></tr>
<tr><td><a href="appdotnet.md" class="module">appdotnet</a></td></tr>
<tr><td><a href="appdotnet_app.md" class="module">appdotnet_app</a></td></tr>
<tr><td><a href="appdotnet_client.md" class="module">appdotnet_client</a></td></tr>
<tr><td><a href="appdotnet_sup.md" class="module">appdotnet_sup</a></td></tr>
<tr><td><a href="authenticated_tests.md" class="module">authenticated_tests</a></td></tr></table>

