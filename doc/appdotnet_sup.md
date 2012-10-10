

#Module appdotnet_sup#
* [Function Index](#index)
* [Function Details](#functions)


Copyright (c) 2012 Erik Hedenstr&ouml;m

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Erik Hedenstr&ouml;m ([`erik@hedenstroem.com`](mailto:erik@hedenstroem.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td><a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td><a href="http://www.erlang.org/doc/man/supervisor.html#start_link-2">supervisor:start_link/2</a></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="init-1"></a>

###init/1##


<pre>init(Args::term()) -> {ok, {{RestartStrategy::<a href="supervisor.md#type-strategy">supervisor:strategy()</a>, MaxR::non_neg_integer(), MaxT::non_neg_integer()}, [ChildSpec::<a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>]}} | ignore</pre>
<br></br>


[supervisor:init/1](http://www.erlang.org/doc/man/supervisor.html#Module:init-1)<a name="start_link-0"></a>

###start_link/0##


<pre>start_link() -&gt; Result</pre>
<ul class="definitions"><li><pre>Result = {ok, pid()} | ignore | {error, StartlinkErr}</pre></li><li><pre>StartlinkErr = {already_started, pid()} | shutdown | term()</pre></li></ul>

[supervisor:start_link/2](http://www.erlang.org/doc/man/supervisor.html#start_link-2)