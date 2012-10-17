%% @author Erik Hedenstr&ouml;m <erik@hedenstroem.com>

-module(anonymous_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

client_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {with, [
             fun authenticate_url_test/1,
             fun retrieve_post_test/1,
             fun retrieve_posts_test/1,
             fun global_stream_test/1,
             fun list_tagged_test/1
            ]
     }
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================
setup() ->
    ok = application:start(appdotnet),
    {ok, Pid} = appdotnet_client:start(),
    Pid.

teardown(Pid) ->
    appdotnet_client:stop(Pid).

authenticate_url_test(Pid) ->
    {ok, URL} = appdotnet_client:q(Pid, authenticate_url, ["TEST", "http://localhost", ["stream","email"]]),
    ?assertEqual("https://alpha.app.net/oauth/authenticate?client_id=TEST&response_type=code&redirect_uri=http%3a%2f%2flocalhost&scope=stream%2cemail",URL).

retrieve_post_test(Pid) ->
    {ok, _Post, Meta} = appdotnet_client:q(Pid, retrieve_post, ["588214"]),
    check_meta(Meta).

retrieve_posts_test(Pid) ->
    {ok, _Posts, Meta} = appdotnet_client:q(Pid, retrieve_posts, ["19697",[]]),
    check_meta(Meta).

global_stream_test(Pid) ->
    {ok, _Posts, Meta} = appdotnet_client:q(Pid, retrieve_global_stream, [[]]),
    check_meta(Meta).

list_tagged_test(Pid) ->
    {ok, _Posts, Meta} = appdotnet_client:q(Pid, retrieve_tagged, ["erlang",[{"count",1}]]),
    check_meta(Meta).

check_meta(Meta) ->
    ?assertEqual(proplists:get_value(<<"code">>, Meta),200).