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
	ssl:start(),
	ibrowse:start().

