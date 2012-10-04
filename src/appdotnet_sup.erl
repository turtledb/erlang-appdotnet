%% @author erikh
%% @doc @todo Add description to appdotnet_sup.

-module(appdotnet_sup).
-behaviour(supervisor).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
-export([start_link/0]).

%% start_link/1
%% ====================================================================
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
          Result when
                   Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
                   SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
                   RestartStrategy :: one_for_all
                       | one_for_one
                       | rest_for_one
                       | simple_one_for_one,
                   ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
                   StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
                   RestartPolicy :: permanent
                       | transient
                       | temporary,
                   Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    Child = {appdotnet_client,{appdotnet_client,start_link,[]},
             temporary,2000,worker,[appdotnet_client]},
    {ok,{{simple_one_for_one,0,1}, [Child]}}.