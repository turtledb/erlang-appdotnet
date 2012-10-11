%% @author Erik Hedenstr&ouml;m <erik@hedenstroem.com>

-module(authenticated_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

client_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {with, [
             fun retrieve_user_test/1,
             fun list_followers_test/1,
             fun check_current_token_test/1
            ]
     }
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================
setup() ->
    ok = application:start(appdotnet),
    {ok, Pid} = appdotnet_client:start(),
    {ok, AccessToken} = application:get_env(appdotnet,access_token),
    {Pid, AccessToken}.

teardown({Pid, _AccessToken}) ->
    appdotnet_client:stop(Pid).

retrieve_user_test({Pid, AccessToken}) ->
    {ok, User, _Meta} = appdotnet_client:q(Pid, retrieve_user, [AccessToken, "@erikh"]),
    Id = proplists:get_value(<<"id">>, User),
    ?assertEqual(<<"19697">>, Id).

list_followers_test({Pid, AccessToken}) ->
    {ok, _Data, _Meta} = appdotnet_client:q(Pid, list_followers, [AccessToken, "@erikh"]).

check_current_token_test({Pid, AccessToken}) ->
    {ok, _Data, _Meta} = appdotnet_client:q(Pid, check_current_token, [AccessToken]).
