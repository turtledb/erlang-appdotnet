%% @author Erik Hedenstr&ouml;m <erik@hedenstroem.com>
%% @copyright 2012 Erik Hedenstr&ouml;m

-module(appdotnet_sup).
-behaviour(supervisor).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
-export([start_link/0]).

%% start_link/0
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#start_link-2">supervisor:start_link/2</a>
-spec start_link() -> 
          Result when
                   Result :: {ok, pid()}
                       | ignore
                       | {error, StartlinkErr},
                   StartlinkErr :: {already_started, pid()} 
                       | shutdown
                       | term().
%% ====================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) ->
          {ok, {{RestartStrategy :: supervisor:strategy(),
                 MaxR            :: non_neg_integer(),
                 MaxT            :: non_neg_integer()},
                [ChildSpec :: supervisor:child_spec()]}}
              | ignore.
%% ====================================================================
init([]) ->
    Child = {appdotnet_client,{appdotnet_client,start_link,[]},
             temporary,2000,worker,[appdotnet_client]},
    {ok,{{simple_one_for_one,0,1}, [Child]}}.