%% Author: erikh
%% Created: 5 okt 2012
%% Description: TODO: Add description to appdotnet
-module(appdotnet).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([authenticate_url/2,authenticate_url/3,access_token/4]).
-export([retrieve_user/2,follow_user/2,unfollow_user/2,following/2,followers/2]).

%%
%% API Functions
%%
authenticate_url(ClientId, RedirectURI) -> authenticate_url(ClientId, RedirectURI, []).
authenticate_url(ClientId, RedirectURI, Scope) ->
    Params = [{"client_id",ClientId},
              {"response_type","code"},
              {"redirect_uri",RedirectURI},
              {"scope", string:join(Scope,",")}
             ],
    {ok,"https://alpha.app.net/oauth/authenticate?" ++ url_encode(Params)}.

access_token(ClientId, ClientSecret, RedirectURI, Code) ->
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
            {ok, binary_to_list(BinaryAccessToken)};
        {ok, StatusCode, Headers, Body} ->
            {error, {list_to_integer(StatusCode), Headers, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

retrieve_user(AccessToken, UserId) ->
    get_resource(AccessToken, get, "/stream/0/users/"++UserId).

follow_user(AccessToken, UserId) ->
    get_resource(AccessToken, post, "/stream/0/users/"++UserId++"/follow").

unfollow_user(AccessToken, UserId) ->
    get_resource(AccessToken, delete, "/stream/0/users/"++UserId++"/follow").

following(AccessToken, UserId) ->
    get_resource(AccessToken, get, "/stream/0/users/"++UserId++"/following").

followers(AccessToken, UserId) ->
    get_resource(AccessToken, get, "/stream/0/users/"++UserId++"/followers").

%%
%% Local Functions
%%
url_encode(Params) ->
    url_encode(Params,"").

url_encode([],[_|Acc]) ->
    Acc;

url_encode([{Key,Value}|Params],Acc) ->
    url_encode(Params, Acc ++ "&" ++ ibrowse_lib:url_encode(Key) ++ "=" ++ ibrowse_lib:url_encode(Value)).

get_resource(AccessToken, Method, Path) ->
    case ibrowse:send_req("https://alpha-api.app.net/"++Path, [{"Authorization","Bearer "++AccessToken}], Method, []) of
        {ok, [$2|_], _Headers, Body} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, StatusCode, Headers, Body} ->
            {error, {list_to_integer(StatusCode), Headers, Body}};
        {error, Reason} ->
            {error, Reason}
    end.