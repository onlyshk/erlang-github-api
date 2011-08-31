%%
%%Copyright (c) <2011>, Kuleshov Alexander <kuleshovmail@gmail.com>
%%All rights reserved.
%%
-module(messages).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([make_gist_comment_message/1]).

%%
%% API Functions
%%

%%
%% @spec make_gist_comment_message(Message) -> Json
%% @doc  - Make json message
%% @type - Message = String()
%% @type - Json = String()
%%
make_gist_comment_message([]) ->
	error;
make_gist_comment_message(Message) ->
	"{ \"body\":\"" ++ Message ++ "\"}".
