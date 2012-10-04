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
-export([retrieve_user/3,follow_user/3,unfollow_user/3,followers/3,following/3]).

start() ->
    start([]).

start(Args) ->
    case whereis(appdotnet_sup) of
        undefined -> application:start(appdotnet);
        _ -> ok
    end,
    supervisor:start_child(appdotnet_sup, [Args]).

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
    gen_server:call(Pid, {resource, AccessToken, get, "/stream/0/users/"++UserId}, ?CALL_TIMEOUT).

follow_user(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, AccessToken, post, "/stream/0/users/"++UserId++"/follow"}, ?CALL_TIMEOUT).

unfollow_user(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, AccessToken, delete, "/stream/0/users/"++UserId++"/follow"}, ?CALL_TIMEOUT).

following(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, AccessToken, get, "/stream/0/users/"++UserId++"/following"}, ?CALL_TIMEOUT).

followers(Pid, AccessToken, UserId) ->
    gen_server:call(Pid, {resource, AccessToken, get, "/stream/0/users/"++UserId++"/followers"}, ?CALL_TIMEOUT).

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
    Params = [{"client_id",ClientId},
              {"response_type","code"},
              {"redirect_uri",RedirectURI},
              {"scope", string:join(Scope,",")}
             ],
    URL = "https://alpha.app.net/oauth/authenticate?" ++ url_encode(Params),
    {reply, {ok, URL}, State};

handle_call({access_token, ClientId, ClientSecret, RedirectURI, Code}, _From, State) ->
    Params = [{"client_id",ClientId},
              {"client_secret",ClientSecret},
              {"grant_type","authorization_code"},
              {"redirect_uri",RedirectURI},
              {"code", Code}
             ],
    case ibrowse:send_req("https://alpha.app.net/oauth/access_token", [{"Content-Type","application/x-www-form-urlencoded"}], post, url_encode(Params)) of
        {ok, [$2|_], _Headers, Body} ->
            Data = jsx:decode(list_to_binary(Body)),
            BinaryAccessToken = proplists:get_value(<<"access_token">>, Data),
            {reply, {ok, binary_to_list(BinaryAccessToken)}, State};
        {ok, StatusCode, Headers, Body} ->
            {reply, {error, {list_to_integer(StatusCode), Headers, Body}}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({resource, AccessToken, Method, Path}, _From, State) ->
    case ibrowse:send_req("https://alpha-api.app.net/"++Path, [{"Authorization","Bearer "++AccessToken}], Method, []) of
        {ok, [$2|_], _Headers, Body} ->
            {reply, {ok, jsx:decode(list_to_binary(Body))}, State};
        {ok, StatusCode, Headers, Body} ->
            {reply, {error, {list_to_integer(StatusCode), Headers, Body}}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

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
url_encode(Params) ->
    url_encode(Params,"").

url_encode([],[_|Acc]) ->
    Acc;

url_encode([{Key,Value}|Params],Acc) ->
    url_encode(Params, Acc ++ "&" ++ ibrowse_lib:url_encode(Key) ++ "=" ++ ibrowse_lib:url_encode(Value)).