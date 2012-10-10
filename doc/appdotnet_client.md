

#Module appdotnet_client#
* [Function Index](#index)
* [Function Details](#functions)


Copyright (c) 2012 Erik Hedenstr&ouml;m

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Erik Hedenstr&ouml;m ([`erik@hedenstroem.com`](mailto:erik@hedenstroem.com)).<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td><a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td><a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td><a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td><a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td><a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a></td></tr><tr><td valign="top"><a href="#q-3">q/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td><a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="code_change-3"></a>

###code_change/3##


<pre>code_change(OldVsn, State::term(), Extra::term()) -&gt; Result</pre>
<ul class="definitions"><li><pre>Result = {ok, NewState::term()} | {error, Reason::term()}</pre></li><li><pre>OldVsn = Vsn | {down, Vsn}</pre></li><li><pre>Vsn = term()</pre></li></ul>

[gen_server:code_change/3](http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3)<a name="handle_call-3"></a>

###handle_call/3##


<pre>handle_call(Request::term(), From::{pid(), Tag::term()}, State::term()) -&gt; Result</pre>
<ul class="definitions"><li><pre>Result = {reply, Reply, NewState} | {reply, Reply, NewState, Timeout} | {reply, Reply, NewState, hibernate} | {noreply, NewState} | {noreply, NewState, Timeout} | {noreply, NewState, hibernate} | {stop, Reason, Reply, NewState} | {stop, Reason, NewState}</pre></li><li><pre>Reply = term()</pre></li><li><pre>NewState = term()</pre></li><li><pre>Timeout = non_neg_integer() | infinity</pre></li><li><pre>Reason = term()</pre></li></ul>

[gen_server:handle_call/3](http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3)<a name="handle_cast-2"></a>

###handle_cast/2##


<pre>handle_cast(Request::term(), State::term()) -&gt; Result</pre>
<ul class="definitions"><li><pre>Result = {noreply, NewState} | {noreply, NewState, Timeout} | {noreply, NewState, hibernate} | {stop, Reason::term(), NewState}</pre></li><li><pre>NewState = term()</pre></li><li><pre>Timeout = non_neg_integer() | infinity</pre></li></ul>

[gen_server:handle_cast/2](http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2)<a name="handle_info-2"></a>

###handle_info/2##


<pre>handle_info(Info::timeout | term(), State::term()) -&gt; Result</pre>
<ul class="definitions"><li><pre>Result = {noreply, NewState} | {noreply, NewState, Timeout} | {noreply, NewState, hibernate} | {stop, Reason::term(), NewState}</pre></li><li><pre>NewState = term()</pre></li><li><pre>Timeout = non_neg_integer() | infinity</pre></li></ul>

[gen_server:handle_info/2](http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2)<a name="init-1"></a>

###init/1##


<pre>init(Args::term()) -&gt; Result</pre>
<ul class="definitions"><li><pre>Result = {ok, State} | {ok, State, Timeout} | {ok, State, hibernate} | {stop, Reason::term()} | ignore</pre></li><li><pre>State = term()</pre></li><li><pre>Timeout = non_neg_integer() | infinity</pre></li></ul>

[gen_server:init/1](http://www.erlang.org/doc/man/gen_server.html#Module:init-1)<a name="q-3"></a>

###q/3##


<pre>q(Pid::pid(), Function::atom(), Args::list()) -&gt; {ok, term()} | {error, term()}</pre>
<br></br>


<a name="start-0"></a>

###start/0##


`start() -> any()`

<a name="start-1"></a>

###start/1##


`start(Args) -> any()`

<a name="start_link-1"></a>

###start_link/1##


`start_link(Args) -> any()`

<a name="stop-1"></a>

###stop/1##


`stop(Pid) -> any()`

<a name="terminate-2"></a>

###terminate/2##


<pre>terminate(Reason, State::term()) -&gt; Any::term()</pre>
<ul class="definitions"><li><pre>Reason = normal | shutdown | {shutdown, term()} | term()</pre></li></ul>

[gen_server:terminate/2](http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2)