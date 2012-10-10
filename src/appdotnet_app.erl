%% @author Erik Hedenstr&ouml;m <erik@hedenstroem.com>
%% @copyright 2012 Erik Hedenstr&ouml;m

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
    case start_deps() of
        {_,[]} ->
            case appdotnet_sup:start_link() of
                {ok, Pid} ->
                    {ok, Pid};
                Error ->
                    Error
            end;
        {_,NotStarted} ->
            {error,{not_started,NotStarted}}
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
-spec start_deps() -> {[atom()],[atom()]}.
start_deps() ->
    lists:partition(fun(App) ->
                            ensure_started(App)
                    end, [crypto, public_key, ssl, ibrowse]).

-spec ensure_started(App :: atom()) -> boolean().
ensure_started(App) ->
    case application:start(App) of
        ok -> true;
        {error, {already_started, App}} -> true;
        {error, _} -> false
    end.