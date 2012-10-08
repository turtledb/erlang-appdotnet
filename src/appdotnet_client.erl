%% @author erikh
%% @doc @todo Add description to appdotnet_client.

-module(appdotnet_client).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CALL_TIMEOUT, 30000). 

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,start/1,stop/1,start_link/1]).

-export([authenticate_url/3,authenticate_url/4,access_token/5]).

-export([retrieve_user/3,follow_user/3,unfollow_user/3,list_following/3,list_followers/3]).
-export([mute_user/3,unmute_user/3,list_muted/2]).
-export([search_for_users/3,list_reposters/3,list_stars/3]).

-export([check_current_token/2]).

start() ->
    start([]).

start(Args) ->
    case whereis(appdotnet_sup) of
        undefined ->
            case application:start(appdotnet) of
                ok ->
                    supervisor:start_child(appdotnet_sup, [Args]);
                Error ->
                    Error
            end;
        _ ->
            supervisor:start_child(appdotnet_sup, [Args])
    end.

stop(Pid) ->
    gen_server:cast(Pid, stop).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

authenticate_url(Pid, ClientId, RedirectURI) ->
    authenticate_url(Pid, ClientId, RedirectURI, []).
authenticate_url(Pid, ClientId, RedirectURI, Scope) ->
    gen_server:call(Pid, {authenticate_url, ClientId, RedirectURI, Scope}, ?CALL_TIMEOUT).

access_token(Pid, ClientId, ClientSecret, RedirectURI, Code) ->
    gen_server:call(Pid, {access_token, ClientId, ClientSecret, RedirectURI, Code}, ?CALL_TIMEOUT).

retrieve_user(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, retrieve_user, [AccessToken,UserId]}, ?CALL_TIMEOUT).

follow_user(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, follow_user, [AccessToken,UserId]}, ?CALL_TIMEOUT).

unfollow_user(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, unfollow_user, [AccessToken,UserId]}, ?CALL_TIMEOUT).

list_following(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, list_following, [AccessToken,UserId]}, ?CALL_TIMEOUT).

list_followers(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, list_followers, [AccessToken,UserId]}, ?CALL_TIMEOUT).

mute_user(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, mute_user, [AccessToken,UserId]}, ?CALL_TIMEOUT).

unmute_user(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, unmute_user, [AccessToken,UserId]}, ?CALL_TIMEOUT).

list_muted(Pid, AccessToken) ->
    gen_server:call(Pid, {resource, list_muted, [AccessToken]}, ?CALL_TIMEOUT).

search_for_users(Pid, AccessToken, Query) ->
    gen_server:call(Pid, {resource, search_for_users, [AccessToken,Query]}, ?CALL_TIMEOUT).

list_reposters(Pid, AccessToken, PostId) ->
    gen_server:call(Pid, {resource, list_reposters, [AccessToken,PostId]}, ?CALL_TIMEOUT).

list_stars(Pid, AccessToken, PostId) ->
    gen_server:call(Pid, {resource, list_stars, [AccessToken,PostId]}, ?CALL_TIMEOUT).

check_current_token(Pid, AccessToken) ->
    gen_server:call(Pid, {resource, check_current_token, [AccessToken]}, ?CALL_TIMEOUT).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) ->
          Result when
                   Result :: {ok, State}
                       | {ok, State, Timeout}
                       | {ok, State, hibernate}
                       | {stop, Reason :: term()}
                       | ignore,
                   State :: term(),
                   Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(_Args) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) ->
          Result when
                   Result :: {reply, Reply, NewState}
                       | {reply, Reply, NewState, Timeout}
                       | {reply, Reply, NewState, hibernate}
                       | {noreply, NewState}
                       | {noreply, NewState, Timeout}
                       | {noreply, NewState, hibernate}
                       | {stop, Reason, Reply, NewState}
                       | {stop, Reason, NewState},
                   Reply :: term(),
                   NewState :: term(),
                   Timeout :: non_neg_integer() | infinity,
                   Reason :: term().
%% ====================================================================
handle_call({authenticate_url, ClientId, RedirectURI, Scope}, _From, State) ->
    {reply, appdotnet:authenticate_url(ClientId, RedirectURI, Scope), State};

handle_call({access_token, ClientId, ClientSecret, RedirectURI, Code}, _From, State) ->
    {reply, appdotnet:access_token(ClientId, ClientSecret, RedirectURI, Code), State};

handle_call({resource, Function, Args}, _From, State) ->
    {reply, erlang:apply(appdotnet, Function, Args), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) ->
          Result when
                   Result :: {noreply, NewState}
                       | {noreply, NewState, Timeout}
                       | {noreply, NewState, hibernate}
                       | {stop, Reason :: term(), NewState},
                   NewState :: term(),
                   Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) ->
          Result when
                   Result :: {noreply, NewState}
                       | {noreply, NewState, Timeout}
                       | {noreply, NewState, hibernate}
                       | {stop, Reason :: term(), NewState},
                   NewState :: term(),
                   Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) ->
          Any :: term() when
                          Reason :: normal
                              | shutdown
                              | {shutdown, term()}
                              | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) ->
          Result when
                   Result :: {ok, NewState :: term()} | {error, Reason :: term()},
                   OldVsn :: Vsn | {down, Vsn},
                   Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
