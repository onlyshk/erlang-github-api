%%
%%Copyright (c) <2011>, Kuleshov Alexander <kuleshovmail@gmail.com>
%%All rights reserved.
%%

-module(auth).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([auth/2]).

%%
%% API Functions
%%

-include("define.hrl").

%% 
%% @spec auth(UserName, Password) -> ok
%% @doc  - Github basic authorization
%% @type - UserName - String()
%% @type - Password - String()
%% ok - atom()
%%
auth(UserName, Password) ->
	 github:init(),
	 ibrowse:send_req(?GITHUB, [], get, [],
					  [{basic_auth, {UserName, Password}},{stream_to, self()}]),
	 ok.

