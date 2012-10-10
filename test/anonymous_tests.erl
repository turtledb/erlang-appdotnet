%% @author Erik Hedenstr&ouml;m <erik@hedenstroem.com>
%% @copyright 2012 Erik Hedenstr&ouml;m

-module(anonymous_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

authorize_url_test() ->
    ok = application:start(appdotnet),
    {ok, Pid} = appdotnet_client:start(),
    {ok, URL} = appdotnet_client:q(Pid, authenticate_url, ["TEST", "http://localhost", ["stream","email"]]),
    ?assertEqual("https://alpha.app.net/oauth/authenticate?client_id=TEST&response_type=code&redirect_uri=http%3a%2f%2flocalhost&scope=stream%2cemail",URL),
    appdotnet_client:stop(Pid).

%% ====================================================================
%% Internal functions
%% ====================================================================
