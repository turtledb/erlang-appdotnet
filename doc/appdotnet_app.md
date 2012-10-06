

#Module appdotnet_app#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


@todo Add description to appdotnet_app.

__Behaviours:__ [`application`](application.md).

__Authors:__ erikh.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-2">start/2</a></td><td><a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td><a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="start-2"></a>

###start/2##


<pre>start(Type::normal | {takeover, Node} | {failover, Node}, Args::term()) -&gt; Result</pre>
<ul class="definitions"><li><pre>Result = {ok, Pid::pid()} | {error, Reason::term()}</pre></li></ul>

[application:start/2](http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2)<a name="stop-1"></a>

###stop/1##


<pre>stop(State::term()) -&gt; ok</pre>
<br></br>


[application:stop/1](http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1)