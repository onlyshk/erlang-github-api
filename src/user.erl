%%
%%Copyright (c) <2011>, Kuleshov Alexander <kuleshovmail@gmail.com>
%%All rights reserved.
%%
-module(user).

-include("define.hrl").

%%==============================================
%%             Github Users
%%==============================================

get_user(User, UserName, Password) ->
    github:init(),
    ibrowse:send_req(?USERS ++ User, [], get, [],
					  [{basic_auth, {UserName, Password}}, 
					   {ssl_options, [{verify, 0}, {depth, 3}]}]).
