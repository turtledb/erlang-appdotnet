%% @author erikh
%% @doc @todo Add description to appdotnet_tests.


-module(appdotnet_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

authorize_url_test() ->
    application:start(appdotnet),
    {ok, ClientId} = application:get_env(appdotnet,client_id),
    {ok, Pid} = appdotnet_client:start(),
    {ok, URL} = appdotnet_client:authenticate_url(Pid, ClientId, "http://localhost", ["stream","email"]),
    ?assertEqual("https://alpha.app.net/oauth/authenticate?client_id=p7wqkeWtqJbqhM5cemehvScLLtRWAWxs&response_type=code&redirect_uri=http%3a%2f%2flocalhost&scope=stream%2cemail",URL),
    appdotnet_client:stop(Pid).

retrieve_user_test() ->
    application:start(appdotnet),
    {ok, AccessToken} = application:get_env(appdotnet,access_token),
    {ok, Pid} = appdotnet_client:start(),
    {ok, Data} = appdotnet_client:retrieve_user(Pid, AccessToken, "@erikh"),
    Id = rget_kv([<<"data">>,<<"id">>], Data),
    ?assertEqual(<<"19697">>,Id),
    appdotnet_client:stop(Pid).

followers_test() ->
    application:start(appdotnet),
    {ok, AccessToken} = application:get_env(appdotnet,access_token),
    {ok, Pid} = appdotnet_client:start(),
    {ok, Data} = appdotnet_client:followers(Pid, AccessToken, "@erikh"),
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
