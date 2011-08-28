%%
%%Copyright (c) <2011>, Kuleshov Alexander <kuleshovmail@gmail.com>
%%All rights reserved.
%%
-module(utils).

%%
%% Exported Functions
%%
-export([clean_quotes/1]).

%%
%% API Functions
%%

%%
%% Remove \" \"
%%
clean_quotes(String) ->
	string:tokens(String, "\"").