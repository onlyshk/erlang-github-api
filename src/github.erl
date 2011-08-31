%%
%%Copyright (c) <2011>, Kuleshov Alexander <kuleshovmail@gmail.com>
%%All rights reserved.
%%

-module(github).

%%
%% Exported Functions
%%
-export([init/0]).

%%
%% API Functions
%%

%%
%% Init ssl and ibrowse
%%
init() ->
	application:start(crypto),
	application:start(ssl),
	ssl:start(),
	ibrowse:start().

