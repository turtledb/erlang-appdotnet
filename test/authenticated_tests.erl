%% @author Erik Hedenstr&ouml;m <erik@hedenstroem.com>


-module(authenticated_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

retrieve_user_test() ->
    application:start(appdotnet),
    {ok, AccessToken} = application:get_env(appdotnet,access_token),
    {ok, Pid} = appdotnet_client:start(),
    {ok, Data} = appdotnet_client:retrieve_user(Pid, AccessToken, "@erikh"),
    Id = rget_kv([<<"data">>,<<"id">>], Data),
    ?assertEqual(<<"19697">>,Id),
    appdotnet_client:stop(Pid).

list_followers_test() ->
    application:start(appdotnet),
    {ok, AccessToken} = application:get_env(appdotnet,access_token),
    {ok, Pid} = appdotnet_client:start(),
    {ok, _Data} = appdotnet_client:list_followers(Pid, AccessToken, "@erikh"),
    appdotnet_client:stop(Pid).

check_current_token_test() ->
    application:start(appdotnet),
    {ok, AccessToken} = application:get_env(appdotnet,access_token),
    {ok, Pid} = appdotnet_client:start(),
    {ok, _Data} = appdotnet_client:check_current_token(Pid, AccessToken),
    appdotnet_client:stop(Pid).

%% ====================================================================
%% Internal functions
%% ====================================================================
rget_kv(Keys,List) ->
    rget_kv(Keys,List,undefined).

rget_kv(_Keys,[],Default) ->
    Default;

rget_kv([Key],List,Default) ->
    proplists:get_value(Key, List, Default);

rget_kv([Key|Keys],List,Default) ->
    rget_kv(Keys,proplists:get_value(Key, List, []),Default).
