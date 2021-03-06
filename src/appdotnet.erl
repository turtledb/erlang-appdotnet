%% @author Erik Hedenstr&ouml;m <erik@hedenstroem.com>
%% @doc Erlang implementation of the App.net <a href="https://github.com/appdotnet/api-spec">API Specification</a>

-module(appdotnet).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([authenticate_url/2,authenticate_url/3,access_token/4]).

-export([retrieve_user/2,follow_user/2,unfollow_user/2,list_following/2,list_followers/2]).
-export([mute_user/2,unmute_user/2,list_muted/1]).
-export([search_for_users/2,list_reposters/2,list_stars/2]).

-export([check_current_token/1]).

-export([create_post/2,create_reply/3,create_complex_post/2]).
-export([retrieve_post/1,delete_post/2]).
-export([retrieve_replies/3,retrieve_posts/2]).
-export([repost_post/2,unrepost_post/2,star_post/2,unstar_post/2]).
-export([retrieve_starred/3,retrieve_mentions/3,retrieve_tagged/2]).
-export([retrieve_personal_stream/2,retrieve_global_stream/1]).

-export([retrieve_filters/1,create_filter/2,delete_filters/1]).
-export([retrieve_filter/2,update_filter/3,delete_filter/2]).

-export_type([http_response/0]).

-type json_term() :: list({binary(), json_term()})
    | list(json_term())
    | true
    | false
    | null
    | integer()
    | float()
    | binary().
-type http_response() :: {ok, Data :: json_term(), Meta :: json_term()} | {error, Reason :: term()}.
-type general_parameters() :: [{string(),string() | non_neg_integer()}].

-define(API_URL, "https://alpha-api.app.net/").
-define(AUTH_URL, "https://alpha.app.net/oauth/authenticate").
-define(TOKEN_URL, "https://alpha.app.net/oauth/access_token").

%%
%% API Functions
%%

%% ---------------------------------------------------------------
%% Authentication
%% ---------------------------------------------------------------

%% @doc Generate an authentication URL with default scope.
%% See <a href="https://github.com/appdotnet/api-spec/blob/master/auth.md">app.net authentication</a> for more information
-spec authenticate_url(ClientId :: string(),
                       RedirectURI :: string()) -> {ok, URL :: string()}.
authenticate_url(ClientId, RedirectURI) -> authenticate_url(ClientId, RedirectURI, []).

%% @doc Generate an authentication URL.
%% See <a href="https://github.com/appdotnet/api-spec/blob/master/auth.md">app.net authentication</a> for more information
-spec authenticate_url(ClientId :: string(),
                       RedirectURI :: string(),
                       Scope :: [string()]) -> {ok, URL :: string()}.
authenticate_url(ClientId, RedirectURI, Scope) ->
    Params = [{"client_id",ClientId},
              {"response_type","code"},
              {"redirect_uri",RedirectURI},
              {"scope", string:join(Scope,",")}
             ],
    {ok,?AUTH_URL ++ "?" ++ url_encode(Params)}.

%% @doc Retrieve an access token.
%% See <a href="https://github.com/appdotnet/api-spec/blob/master/auth.md">app.net authentication</a> for more information
-spec access_token(ClientId :: string(),
                   ClientSecret :: string(),
                   RedirectURI :: string(),
                   Code :: string()) -> {ok, AccessToken :: string()} | {error, Reason :: term()}.
access_token(ClientId, ClientSecret, RedirectURI, Code) ->
    Params = [{"client_id",ClientId},
              {"client_secret",ClientSecret},
              {"grant_type","authorization_code"},
              {"redirect_uri",RedirectURI},
              {"code", Code}
             ],
    case ibrowse:send_req(?TOKEN_URL, [{"Content-Type","application/x-www-form-urlencoded"}], post, url_encode(Params)) of
        {ok, [$2|_], _Headers, Body} ->
            Data = jsx:decode(list_to_binary(Body)),
            BinaryAccessToken = proplists:get_value(<<"access_token">>, Data),
            {ok, binary_to_list(BinaryAccessToken)};
        {ok, StatusCode, Headers, Body} ->
            {error, {list_to_integer(StatusCode), Headers, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% ---------------------------------------------------------------
%% Users
%% ---------------------------------------------------------------

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#retrieve-a-user">Retrieve a User</a>
-spec retrieve_user(AccessToken :: string(), UserId :: string()) -> http_response().
retrieve_user(AccessToken, UserId) ->
    get_resource(AccessToken, get, "/stream/0/users/"++UserId).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#follow-a-user">Follow a User</a>
-spec follow_user(AccessToken :: string(), UserId :: string()) -> http_response().
follow_user(AccessToken, UserId) ->
    get_resource(AccessToken, post, "/stream/0/users/"++UserId++"/follow").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#unfollow-a-user">Unfollow a User</a>
-spec unfollow_user(AccessToken :: string(), UserId :: string()) -> http_response().
unfollow_user(AccessToken, UserId) ->
    get_resource(AccessToken, delete, "/stream/0/users/"++UserId++"/follow").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-a-user-is-following">List users a User is following</a>
-spec list_following(AccessToken :: string(), UserId :: string()) -> http_response().
list_following(AccessToken, UserId) ->
    get_resource(AccessToken, get, "/stream/0/users/"++UserId++"/following").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-following-a-user">List users following a User</a>
-spec list_followers(AccessToken :: string(), UserId :: string()) -> http_response().
list_followers(AccessToken, UserId) ->
    get_resource(AccessToken, get, "/stream/0/users/"++UserId++"/followers").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#mute-a-user">Mute a User</a>
-spec mute_user(AccessToken :: string(), UserId :: string()) -> http_response().
mute_user(AccessToken, UserId) ->
    get_resource(AccessToken, post, "/stream/0/users/"++UserId++"/mute").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#unmute-a-user">Unmute a User</a>
-spec unmute_user(AccessToken :: string(), UserId :: string()) -> http_response().
unmute_user(AccessToken, UserId) ->
    get_resource(AccessToken, delete, "/stream/0/users/"++UserId++"/mute").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-muted-users">List muted Users</a>
-spec list_muted(AccessToken :: string()) -> http_response().
list_muted(AccessToken) ->
    get_resource(AccessToken, get, "/stream/0/users/me/muted").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#search-for-users">Search for Users</a>
-spec search_for_users(AccessToken :: string(), Query :: string()) -> http_response().
search_for_users(AccessToken, Query) ->
    get_resource(AccessToken, get, "/stream/0/users/search?q=" ++ ibrowse_lib:url_encode(Query)).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-who-have-reposted-a-post">List Users who have reposted a Post</a>
-spec list_reposters(AccessToken :: string(), PostId :: string()) -> http_response().
list_reposters(AccessToken, PostId) ->
    get_resource(AccessToken, get, "/stream/0/posts/"++PostId++"/reposters").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-who-have-starred-a-post">List Users who have starred a Post</a>
-spec list_stars(AccessToken :: string(), PostId :: string()) -> http_response().
list_stars(AccessToken, PostId) ->
    get_resource(AccessToken, get, "/stream/0/posts/"++PostId++"/stars").

%% ---------------------------------------------------------------
%% Token
%% ---------------------------------------------------------------

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/token.md#retrieve-current-token">Check current Token</a>
-spec check_current_token(AccessToken :: string()) -> http_response().
check_current_token(AccessToken) ->
    get_resource(AccessToken, get, "/stream/0/token").

%% ---------------------------------------------------------------
%% Posts
%% ---------------------------------------------------------------

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#create-a-post">Create a Post</a>
-spec create_post(AccessToken :: string(), Text :: string()) -> http_response().
create_post(AccessToken, Text) ->
    Path = "/stream/0/posts",
    Headers = [{"Content-Type","application/x-www-form-urlencoded"},{"Authorization","Bearer "++AccessToken}],
    Params = [{"text",Text}],
    send_req(?API_URL++Path, Headers, post, url_encode(Params)).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#create-a-post">Create a Reply</a>
-spec create_reply(AccessToken :: string(), Text :: string(), ReplyTo :: string()) -> http_response().
create_reply(AccessToken, Text, ReplyTo) ->
    Path = "/stream/0/posts",
    Headers = [{"Content-Type","application/x-www-form-urlencoded"},{"Authorization","Bearer "++AccessToken}],
    Params = [{"text",Text},{"reply_to",ReplyTo}],
    send_req(?API_URL++Path, Headers, post, url_encode(Params)).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#create-a-post">Create a complex Post</a>
-spec create_complex_post(AccessToken :: string(), Post :: json_term()) -> http_response().
create_complex_post(AccessToken, Post) ->
    Path = "/stream/0/posts",
    Headers = [{"Content-Type","application/json"},{"Authorization","Bearer "++AccessToken}],
    send_req(?API_URL++Path, Headers, post, jsx:encode(Post,[relax])).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-a-post">Retrieve a Post</a>
-spec retrieve_post(PostId :: string()) -> http_response().
retrieve_post(PostId) ->
    get_resource(get, "/stream/0/posts/"++PostId).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#delete-a-post">Delete a Post</a>
-spec delete_post(AccessToken :: string(), PostId :: string()) -> http_response().
delete_post(AccessToken, PostId) ->
    get_resource(AccessToken, delete, "/stream/0/posts/"++PostId).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-the-replies-to-a-post">Retrieve the replies to a Post</a>.
-spec retrieve_replies(AccessToken :: string(), PostId :: string(), GeneralParameters :: general_parameters()) -> http_response().
retrieve_replies(AccessToken, PostId, GeneralParameters) ->
    get_resource(AccessToken, get, "/stream/0/posts/"++PostId++"/replies?"++url_encode(GeneralParameters)).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-posts-created-by-a-user">Retrieve Posts created by a User</a>
-spec retrieve_posts(UserId :: string(), GeneralParameters :: general_parameters()) -> http_response().
retrieve_posts(UserId, GeneralParameters) ->
    get_resource(get, "/stream/0/users/"++UserId++"/posts?"++url_encode(GeneralParameters)).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#repost-a-post">Repost a Post</a>
-spec repost_post(AccessToken :: string(), PostId :: string()) -> http_response().
repost_post(AccessToken, PostId) ->
    get_resource(AccessToken, post, "/stream/0/posts/"++PostId++"/repost").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#unrepost-a-post">Unrepost a Post</a>
-spec unrepost_post(AccessToken :: string(), PostId :: string()) -> http_response().
unrepost_post(AccessToken, PostId) ->
    get_resource(AccessToken, delete, "/stream/0/posts/"++PostId++"/repost").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#star-a-post">Star a Post</a>
-spec star_post(AccessToken :: string(), PostId :: string()) -> http_response().
star_post(AccessToken, PostId) ->
    get_resource(AccessToken, post, "/stream/0/posts/"++PostId++"/star").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#unstar-a-post">Unstar a Post</a>
-spec unstar_post(AccessToken :: string(), PostId :: string()) -> http_response().
unstar_post(AccessToken, PostId) ->
    get_resource(AccessToken, delete, "/stream/0/posts/"++PostId++"/star").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-posts-starred-by-a-user">Retrieve Posts starred by a User</a>
-spec retrieve_starred(AccessToken :: string(), UserId :: string(), GeneralParameters :: general_parameters()) -> http_response().
retrieve_starred(AccessToken, UserId, GeneralParameters) ->
    get_resource(AccessToken, get, "/stream/0/users/"++UserId++"/stars?"++url_encode(GeneralParameters)).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-posts-mentioning-a-user">Retrieve Posts mentioning a User</a>
-spec retrieve_mentions(AccessToken :: string(), UserId :: string(), GeneralParameters :: general_parameters()) -> http_response().
retrieve_mentions(AccessToken, UserId, GeneralParameters) ->
    get_resource(AccessToken, get, "/stream/0/users/"++UserId++"/mentions?"++url_encode(GeneralParameters)).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-a-users-personalized-stream">Retrieve a User's personalized stream</a>
-spec retrieve_personal_stream(AccessToken :: string(), GeneralParameters :: general_parameters()) -> http_response().
retrieve_personal_stream(AccessToken, GeneralParameters) ->
    get_resource(AccessToken, get, "/stream/0/posts/stream?"++url_encode(GeneralParameters)).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-the-global-stream">Retrieve the Global stream</a>
-spec retrieve_global_stream(GeneralParameters :: general_parameters()) -> http_response().
retrieve_global_stream(GeneralParameters) ->
    get_resource(get, "/stream/0/posts/stream/global?"++url_encode(GeneralParameters)).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-tagged-posts">Retrieve tagged Posts</a>
-spec retrieve_tagged(Hashtag :: string(), GeneralParameters :: general_parameters()) -> http_response().
retrieve_tagged(Hashtag, GeneralParameters) ->
    get_resource(get, "/stream/0/posts/tag/"++Hashtag++"?"++url_encode(GeneralParameters)).

%% ---------------------------------------------------------------
%% Filters
%% ---------------------------------------------------------------

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#get-current-users-filters">Get current user's Filters</a>
-spec retrieve_filters(AccessToken :: string()) -> http_response().
retrieve_filters(AccessToken) ->
    get_resource(AccessToken, get, "/stream/0/filters").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#create-a-filter">Create a Filter</a>
-spec create_filter(AccessToken :: string(), Filter :: json_term()) -> http_response().
create_filter(AccessToken, Filter) ->
    Path = "/stream/0/filters",
    Headers = [{"Content-Type","application/json"},{"Authorization","Bearer "++AccessToken}],
    send_req(?API_URL++Path, Headers, post, jsx:encode(Filter,[relax])).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#delete-all-of-the-current-users-filters">Delete all of the current user's Filters</a>
-spec delete_filters(AccessToken :: string()) -> http_response().
delete_filters(AccessToken) ->
    get_resource(AccessToken, delete, "/stream/0/filters").

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#retrieve-a-filter">Retrieve a Filter</a>
-spec retrieve_filter(AccessToken :: string(), FilterId :: string()) -> http_response().
retrieve_filter(AccessToken,FilterId) ->
    get_resource(AccessToken, get, "/stream/0/filters/"++FilterId).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#update-a-filter">Update a Filter</a>
-spec update_filter(AccessToken :: string(), FilterId :: string(), Filter :: json_term()) -> http_response().
update_filter(AccessToken, FilterId, Filter) ->
    Path = "/stream/0/filters/"++FilterId,
    Headers = [{"Content-Type","application/json"},{"Authorization","Bearer "++AccessToken}],
    send_req(?API_URL++Path, Headers, put, jsx:encode(Filter,[relax])).

%% @doc <a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#delete-a-filter">Delete a Filter</a>
-spec delete_filter(AccessToken :: string(), FilterId :: string()) -> http_response().
delete_filter(AccessToken,FilterId) ->
    get_resource(AccessToken, delete, "/stream/0/filters/"++FilterId).

%%
%% Local Functions
%%
-spec url_encode(Params :: [{string(),string()}]) -> string().
url_encode(Params) ->
    url_encode(Params,"").

-spec url_encode(Params :: [{Key :: string(), Value :: string() | integer()}],Acc :: string()) -> string().
url_encode([],[]) ->
    [];

url_encode([],[_|Acc]) ->
    Acc;

url_encode([{Key,Value}|Params],Acc) when is_number(Value) ->
    url_encode([{Key,erlang:integer_to_list(Value)}|Params],Acc);

url_encode([{Key,Value}|Params],Acc) ->
    url_encode(Params, Acc ++ "&" ++ ibrowse_lib:url_encode(Key) ++ "=" ++ ibrowse_lib:url_encode(Value)).

-spec get_resource(Method :: get | post | delete, Path :: string()) -> http_response().
get_resource(Method, Path) ->
    send_req(?API_URL++Path, [], Method, []).

-spec get_resource(AccessToken :: string(), Method :: get | post | delete, Path :: string()) -> http_response().
get_resource(AccessToken, Method, Path) ->
    send_req(?API_URL++Path, [{"Authorization","Bearer "++AccessToken}], Method, []).

-spec send_req(URL :: string(), Headers :: [{string(),string()}], Method :: get | post | delete, ReqBody :: string() | binary()) -> http_response().
send_req(URL, Headers, Method, ReqBody) ->
    case ibrowse:send_req(URL, [{"X-ADN-Migration-Overrides","response_envelope=1"} | Headers], Method, ReqBody) of
        {ok, [$2|_], _Headers, Body} ->
            Response = jsx:decode(list_to_binary(Body)),
            {ok, proplists:get_value(<<"data">>, Response, []), proplists:get_value(<<"meta">>, Response, [])};
        {ok, StatusCode, Headers, Body} ->
            {error, {list_to_integer(StatusCode), Headers, Body}};
        {error, Reason} ->
            {error, Reason}
    end.