

#erlang-appdotnet#


Copyright (c) 2012 Erik Hedenstršm

__Version:__ 1.0.0


__Authors:__ Erik Hedenstršm ([`erik@hedenstroem.com`](mailto:erik@hedenstroem.com)).

[![Build Status](https://secure.travis-ci.org/ehedenst/erlang-appdotnet.png)](http://travis-ci.org/ehedenst/erlang-appdotnet)

###<a name="Quick_Start">Quick Start</a>##


<pre>
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(ibrowse),
    {ok, Data} = appdotnet:retrieve_user("[ACCESS TOKEN]", "@erikh").
</pre>

or

<pre>
    application:start(appdotnet),
    {ok, Pid} = appdotnet_client:start(),
    {ok, Data} = appdotnet:retrieve_user(Pid,"[ACCESS TOKEN]", "@erikh").
</pre>


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="anonymous_tests.md" class="module">anonymous_tests</a></td></tr>
<tr><td><a href="appdotnet.md" class="module">appdotnet</a></td></tr>
<tr><td><a href="appdotnet_app.md" class="module">appdotnet_app</a></td></tr>
<tr><td><a href="appdotnet_client.md" class="module">appdotnet_client</a></td></tr>
<tr><td><a href="appdotnet_sup.md" class="module">appdotnet_sup</a></td></tr>
<tr><td><a href="authenticated_tests.md" class="module">authenticated_tests</a></td></tr></table>

