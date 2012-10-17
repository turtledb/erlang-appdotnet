

#Module appdotnet#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Erlang implementation of the App.net [API Specification](https://github.com/appdotnet/api-spec)

__Authors:__ Erik Hedenstr&ouml;m ([`erik@hedenstroem.com`](mailto:erik@hedenstroem.com)).
<a name="types"></a>

##Data Types##




###<a name="type-general_parameters">general_parameters()</a>##



<pre>general_parameters() = [{string(), string() | non_neg_integer()}]</pre>



###<a name="type-http_response">http_response()</a>##



<pre>http_response() = {ok, Data::<a href="#type-json_term">json_term()</a>, Meta::<a href="#type-json_term">json_term()</a>} | {error, Reason::term()}</pre>



###<a name="type-json_term">json_term()</a>##



<pre>json_term() = [{binary(), <a href="#type-json_term">json_term()</a>}] | [<a href="#type-json_term">json_term()</a>] | true | false | null | integer() | float() | binary()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#access_token-4">access_token/4</a></td><td>Retrieve an access token.</td></tr><tr><td valign="top"><a href="#authenticate_url-2">authenticate_url/2</a></td><td>Generate an authentication URL with default scope.</td></tr><tr><td valign="top"><a href="#authenticate_url-3">authenticate_url/3</a></td><td>Generate an authentication URL.</td></tr><tr><td valign="top"><a href="#check_current_token-1">check_current_token/1</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/token.md#retrieve-current-token">Check current Token</a></td></tr><tr><td valign="top"><a href="#create_complex_post-2">create_complex_post/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#create-a-post">Create a complex Post</a></td></tr><tr><td valign="top"><a href="#create_filter-2">create_filter/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#create-a-filter">Create a Filter</a></td></tr><tr><td valign="top"><a href="#create_post-2">create_post/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#create-a-post">Create a Post</a></td></tr><tr><td valign="top"><a href="#create_reply-3">create_reply/3</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#create-a-post">Create a Reply</a></td></tr><tr><td valign="top"><a href="#delete_filter-2">delete_filter/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#delete-a-filter">Delete a Filter</a></td></tr><tr><td valign="top"><a href="#delete_filters-1">delete_filters/1</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#delete-all-of-the-current-users-filters">Delete all of the current user's Filters</a></td></tr><tr><td valign="top"><a href="#delete_post-2">delete_post/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#delete-a-post">Delete a Post</a></td></tr><tr><td valign="top"><a href="#follow_user-2">follow_user/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#follow-a-user">Follow a User</a></td></tr><tr><td valign="top"><a href="#list_followers-2">list_followers/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-following-a-user">List users following a User</a></td></tr><tr><td valign="top"><a href="#list_following-2">list_following/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-a-user-is-following">List users a User is following</a></td></tr><tr><td valign="top"><a href="#list_muted-1">list_muted/1</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-muted-users">List muted Users</a></td></tr><tr><td valign="top"><a href="#list_reposters-2">list_reposters/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-who-have-reposted-a-post">List Users who have reposted a Post</a></td></tr><tr><td valign="top"><a href="#list_stars-2">list_stars/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-who-have-starred-a-post">List Users who have starred a Post</a></td></tr><tr><td valign="top"><a href="#mute_user-2">mute_user/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#mute-a-user">Mute a User</a></td></tr><tr><td valign="top"><a href="#repost_post-2">repost_post/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#repost-a-post">Repost a Post</a></td></tr><tr><td valign="top"><a href="#retrieve_filter-2">retrieve_filter/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#retrieve-a-filter">Retrieve a Filter</a></td></tr><tr><td valign="top"><a href="#retrieve_filters-1">retrieve_filters/1</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#get-current-users-filters">Get current user's Filters</a></td></tr><tr><td valign="top"><a href="#retrieve_global_stream-1">retrieve_global_stream/1</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-the-global-stream">Retrieve the Global stream</a></td></tr><tr><td valign="top"><a href="#retrieve_mentions-3">retrieve_mentions/3</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-posts-mentioning-a-user">Retrieve Posts mentioning a User</a></td></tr><tr><td valign="top"><a href="#retrieve_personal_stream-2">retrieve_personal_stream/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-a-users-personalized-stream">Retrieve a User's personalized stream</a></td></tr><tr><td valign="top"><a href="#retrieve_post-1">retrieve_post/1</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-a-post">Retrieve a Post</a></td></tr><tr><td valign="top"><a href="#retrieve_posts-2">retrieve_posts/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-posts-created-by-a-user">Retrieve Posts created by a User</a></td></tr><tr><td valign="top"><a href="#retrieve_replies-3">retrieve_replies/3</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-the-replies-to-a-post">Retrieve the replies to a Post</a>.</td></tr><tr><td valign="top"><a href="#retrieve_starred-3">retrieve_starred/3</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-posts-starred-by-a-user">Retrieve Posts starred by a User</a></td></tr><tr><td valign="top"><a href="#retrieve_tagged-2">retrieve_tagged/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-tagged-posts">Retrieve tagged Posts</a></td></tr><tr><td valign="top"><a href="#retrieve_user-2">retrieve_user/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#retrieve-a-user">Retrieve a User</a></td></tr><tr><td valign="top"><a href="#search_for_users-2">search_for_users/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#search-for-users">Search for Users</a></td></tr><tr><td valign="top"><a href="#star_post-2">star_post/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#star-a-post">Star a Post</a></td></tr><tr><td valign="top"><a href="#unfollow_user-2">unfollow_user/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#unfollow-a-user">Unfollow a User</a></td></tr><tr><td valign="top"><a href="#unmute_user-2">unmute_user/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/users.md#unmute-a-user">Unmute a User</a></td></tr><tr><td valign="top"><a href="#unrepost_post-2">unrepost_post/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#unrepost-a-post">Unrepost a Post</a></td></tr><tr><td valign="top"><a href="#unstar_post-2">unstar_post/2</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#unstar-a-post">Unstar a Post</a></td></tr><tr><td valign="top"><a href="#update_filter-3">update_filter/3</a></td><td><a href="https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#update-a-filter">Update a Filter</a></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="access_token-4"></a>

###access_token/4##


<pre>access_token(ClientId::string(), ClientSecret::string(), RedirectURI::string(), Code::string()) -&gt; {ok, AccessToken::string()} | {error, Reason::term()}</pre>
<br></br>


Retrieve an access token.
See [app.net authentication](https://github.com/appdotnet/api-spec/blob/master/auth.md) for more information<a name="authenticate_url-2"></a>

###authenticate_url/2##


<pre>authenticate_url(ClientId::string(), RedirectURI::string()) -&gt; {ok, URL::string()}</pre>
<br></br>


Generate an authentication URL with default scope.
See [app.net authentication](https://github.com/appdotnet/api-spec/blob/master/auth.md) for more information<a name="authenticate_url-3"></a>

###authenticate_url/3##


<pre>authenticate_url(ClientId::string(), RedirectURI::string(), Scope::[string()]) -&gt; {ok, URL::string()}</pre>
<br></br>


Generate an authentication URL.
See [app.net authentication](https://github.com/appdotnet/api-spec/blob/master/auth.md) for more information<a name="check_current_token-1"></a>

###check_current_token/1##


<pre>check_current_token(AccessToken::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Check current Token](https://github.com/appdotnet/api-spec/blob/master/resources/token.md#retrieve-current-token)<a name="create_complex_post-2"></a>

###create_complex_post/2##


<pre>create_complex_post(AccessToken::string(), Post::<a href="#type-json_term">json_term()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Create a complex Post](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#create-a-post)<a name="create_filter-2"></a>

###create_filter/2##


<pre>create_filter(AccessToken::string(), Filter::<a href="#type-json_term">json_term()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Create a Filter](https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#create-a-filter)<a name="create_post-2"></a>

###create_post/2##


<pre>create_post(AccessToken::string(), Text::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Create a Post](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#create-a-post)<a name="create_reply-3"></a>

###create_reply/3##


<pre>create_reply(AccessToken::string(), Text::string(), ReplyTo::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Create a Reply](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#create-a-post)<a name="delete_filter-2"></a>

###delete_filter/2##


<pre>delete_filter(AccessToken::string(), FilterId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Delete a Filter](https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#delete-a-filter)<a name="delete_filters-1"></a>

###delete_filters/1##


<pre>delete_filters(AccessToken::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Delete all of the current user's Filters](https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#delete-all-of-the-current-users-filters)<a name="delete_post-2"></a>

###delete_post/2##


<pre>delete_post(AccessToken::string(), PostId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Delete a Post](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#delete-a-post)<a name="follow_user-2"></a>

###follow_user/2##


<pre>follow_user(AccessToken::string(), UserId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Follow a User](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#follow-a-user)<a name="list_followers-2"></a>

###list_followers/2##


<pre>list_followers(AccessToken::string(), UserId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[List users following a User](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-following-a-user)<a name="list_following-2"></a>

###list_following/2##


<pre>list_following(AccessToken::string(), UserId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[List users a User is following](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-a-user-is-following)<a name="list_muted-1"></a>

###list_muted/1##


<pre>list_muted(AccessToken::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[List muted Users](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-muted-users)<a name="list_reposters-2"></a>

###list_reposters/2##


<pre>list_reposters(AccessToken::string(), PostId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[List Users who have reposted a Post](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-who-have-reposted-a-post)<a name="list_stars-2"></a>

###list_stars/2##


<pre>list_stars(AccessToken::string(), PostId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[List Users who have starred a Post](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#list-users-who-have-starred-a-post)<a name="mute_user-2"></a>

###mute_user/2##


<pre>mute_user(AccessToken::string(), UserId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Mute a User](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#mute-a-user)<a name="repost_post-2"></a>

###repost_post/2##


<pre>repost_post(AccessToken::string(), PostId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Repost a Post](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#repost-a-post)<a name="retrieve_filter-2"></a>

###retrieve_filter/2##


<pre>retrieve_filter(AccessToken::string(), FilterId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve a Filter](https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#retrieve-a-filter)<a name="retrieve_filters-1"></a>

###retrieve_filters/1##


<pre>retrieve_filters(AccessToken::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Get current user's Filters](https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#get-current-users-filters)<a name="retrieve_global_stream-1"></a>

###retrieve_global_stream/1##


<pre>retrieve_global_stream(GeneralParameters::<a href="#type-general_parameters">general_parameters()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve the Global stream](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-the-global-stream)<a name="retrieve_mentions-3"></a>

###retrieve_mentions/3##


<pre>retrieve_mentions(AccessToken::string(), UserId::string(), GeneralParameters::<a href="#type-general_parameters">general_parameters()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve Posts mentioning a User](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-posts-mentioning-a-user)<a name="retrieve_personal_stream-2"></a>

###retrieve_personal_stream/2##


<pre>retrieve_personal_stream(AccessToken::string(), GeneralParameters::<a href="#type-general_parameters">general_parameters()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve a User's personalized stream](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-a-users-personalized-stream)<a name="retrieve_post-1"></a>

###retrieve_post/1##


<pre>retrieve_post(PostId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve a Post](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-a-post)<a name="retrieve_posts-2"></a>

###retrieve_posts/2##


<pre>retrieve_posts(UserId::string(), GeneralParameters::<a href="#type-general_parameters">general_parameters()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve Posts created by a User](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-posts-created-by-a-user)<a name="retrieve_replies-3"></a>

###retrieve_replies/3##


<pre>retrieve_replies(AccessToken::string(), PostId::string(), GeneralParameters::<a href="#type-general_parameters">general_parameters()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve the replies to a Post](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-the-replies-to-a-post).<a name="retrieve_starred-3"></a>

###retrieve_starred/3##


<pre>retrieve_starred(AccessToken::string(), UserId::string(), GeneralParameters::<a href="#type-general_parameters">general_parameters()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve Posts starred by a User](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-posts-starred-by-a-user)<a name="retrieve_tagged-2"></a>

###retrieve_tagged/2##


<pre>retrieve_tagged(Hashtag::string(), GeneralParameters::<a href="#type-general_parameters">general_parameters()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve tagged Posts](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#retrieve-tagged-posts)<a name="retrieve_user-2"></a>

###retrieve_user/2##


<pre>retrieve_user(AccessToken::string(), UserId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Retrieve a User](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#retrieve-a-user)<a name="search_for_users-2"></a>

###search_for_users/2##


<pre>search_for_users(AccessToken::string(), Query::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Search for Users](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#search-for-users)<a name="star_post-2"></a>

###star_post/2##


<pre>star_post(AccessToken::string(), PostId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Star a Post](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#star-a-post)<a name="unfollow_user-2"></a>

###unfollow_user/2##


<pre>unfollow_user(AccessToken::string(), UserId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Unfollow a User](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#unfollow-a-user)<a name="unmute_user-2"></a>

###unmute_user/2##


<pre>unmute_user(AccessToken::string(), UserId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Unmute a User](https://github.com/appdotnet/api-spec/blob/master/resources/users.md#unmute-a-user)<a name="unrepost_post-2"></a>

###unrepost_post/2##


<pre>unrepost_post(AccessToken::string(), PostId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Unrepost a Post](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#unrepost-a-post)<a name="unstar_post-2"></a>

###unstar_post/2##


<pre>unstar_post(AccessToken::string(), PostId::string()) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Unstar a Post](https://github.com/appdotnet/api-spec/blob/master/resources/posts.md#unstar-a-post)<a name="update_filter-3"></a>

###update_filter/3##


<pre>update_filter(AccessToken::string(), FilterId::string(), Filter::<a href="#type-json_term">json_term()</a>) -> <a href="#type-http_response">http_response()</a></pre>
<br></br>


[Update a Filter](https://github.com/appdotnet/api-spec/blob/master/resources/filters.md#update-a-filter)