%% @author erikh
%% @doc @todo Add description to appdotnet_app.

-module(appdotnet_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
-spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
          Result when 
                   Result :: {ok, Pid :: pid()} | {error, Reason :: term()}.
%% ====================================================================
start(_Type, _Args) ->
    start_ssl(),
    case appdotnet_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.


%% stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
-spec stop(State :: term()) -> ok.
%% ====================================================================
stop(_State) ->
    ok.


%% ====================================================================
%% Internal functions
%% ====================================================================

start_ssl() ->
    ok = ensure_started(crypto),
    ok = ensure_started(public_key),
    ok = ensure_started(ssl),
    ok = ensure_started(ibrowse).


ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok;
        Err -> Err
    end.