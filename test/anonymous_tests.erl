%% @author Erik Hedenstr&ouml;m <erik@hedenstroem.com>
%% @doc @todo Add description to appdotnet_tests.


-module(anonymous_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

authorize_url_test() ->
    application:start(appdotnet),
    {ok, Pid} = appdotnet_client:start(),
    {ok, URL} = appdotnet_client:authenticate_url(Pid, "TEST", "http://localhost", ["stream","email"]),
    ?assertEqual("https://alpha.app.net/oauth/authenticate?client_id=TEST&response_type=code&redirect_uri=http%3a%2f%2flocalhost&scope=stream%2cemail",URL),
    appdotnet_client:stop(Pid).

%% ====================================================================
%% Internal functions
%% ====================================================================
