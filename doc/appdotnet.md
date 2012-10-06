

#Module appdotnet#
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#access_token-4">access_token/4</a></td><td></td></tr><tr><td valign="top"><a href="#authenticate_url-2">authenticate_url/2</a></td><td></td></tr><tr><td valign="top"><a href="#authenticate_url-3">authenticate_url/3</a></td><td></td></tr><tr><td valign="top"><a href="#check_current_token-1">check_current_token/1</a></td><td></td></tr><tr><td valign="top"><a href="#follow_user-2">follow_user/2</a></td><td></td></tr><tr><td valign="top"><a href="#list_followers-2">list_followers/2</a></td><td></td></tr><tr><td valign="top"><a href="#list_following-2">list_following/2</a></td><td></td></tr><tr><td valign="top"><a href="#list_muted-1">list_muted/1</a></td><td></td></tr><tr><td valign="top"><a href="#list_reposters-2">list_reposters/2</a></td><td></td></tr><tr><td valign="top"><a href="#list_stars-2">list_stars/2</a></td><td></td></tr><tr><td valign="top"><a href="#mute_user-2">mute_user/2</a></td><td></td></tr><tr><td valign="top"><a href="#retrieve_user-2">retrieve_user/2</a></td><td></td></tr><tr><td valign="top"><a href="#search_for_users-2">search_for_users/2</a></td><td></td></tr><tr><td valign="top"><a href="#unfollow_user-2">unfollow_user/2</a></td><td></td></tr><tr><td valign="top"><a href="#unmute_user-2">unmute_user/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="access_token-4"></a>

###access_token/4##


`access_token(ClientId, ClientSecret, RedirectURI, Code) -> any()`

<a name="authenticate_url-2"></a>

###authenticate_url/2##


`authenticate_url(ClientId, RedirectURI) -> any()`

<a name="authenticate_url-3"></a>

###authenticate_url/3##


`authenticate_url(ClientId, RedirectURI, Scope) -> any()`

<a name="check_current_token-1"></a>

###check_current_token/1##


`check_current_token(AccessToken) -> any()`

<a name="follow_user-2"></a>

###follow_user/2##


`follow_user(AccessToken, UserId) -> any()`

<a name="list_followers-2"></a>

###list_followers/2##


`list_followers(AccessToken, UserId) -> any()`

<a name="list_following-2"></a>

###list_following/2##


`list_following(AccessToken, UserId) -> any()`

<a name="list_muted-1"></a>

###list_muted/1##


`list_muted(AccessToken) -> any()`

<a name="list_reposters-2"></a>

###list_reposters/2##


`list_reposters(AccessToken, PostId) -> any()`

<a name="list_stars-2"></a>

###list_stars/2##


`list_stars(AccessToken, PostId) -> any()`

<a name="mute_user-2"></a>

###mute_user/2##


`mute_user(AccessToken, UserId) -> any()`

<a name="retrieve_user-2"></a>

###retrieve_user/2##


`retrieve_user(AccessToken, UserId) -> any()`

<a name="search_for_users-2"></a>

###search_for_users/2##


`search_for_users(AccessToken, Query) -> any()`

<a name="unfollow_user-2"></a>

###unfollow_user/2##


`unfollow_user(AccessToken, UserId) -> any()`

<a name="unmute_user-2"></a>

###unmute_user/2##


`unmute_user(AccessToken, UserId) -> any()`

