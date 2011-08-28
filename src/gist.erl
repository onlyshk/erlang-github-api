%%
%%Copyright (c) <2011>, Kuleshov Alexander <kuleshovmail@gmail.com>
%%All rights reserved.
%%

-module(gist).

%%
%% Exported Functions
%%
-export([get_gist_pull_url/1]).
-export([get_gist_content/1]).
-export([get_gist_description/1]).

-define(GIST, "https://api.github.com/gists/").

%%
%% API Functions
%%

%%==============================================
%%             Github Gist
%%==============================================
%%
%% Get gist from github
%% @Id - Gist id
%%
get_gist(Id) ->
	github:init(),
	ibrowse:send_req(?GIST ++ integer_to_list(Id),[], get).
%%
%% Get gist content
%% @Id - gist id
%%
get_gist_content(Id) ->
	GetGist = get_gist(Id),
	case GetGist of
		{ok, "200", _, Content} ->
			Content;
		_ ->
			io:format("Gist getting error")
	end.

%%
%% @spec get_gist_description(Id) -> ["Description"]
%% @doc - Get gist description
%% @type - Id = Int
%% @type - Description = String
%%
get_gist_description(Id) ->
	GetGist = get_gist(Id),
	case GetGist of
		{ok, "200", _, Content} ->
			SplitingPart = string:tokens(Content, ":"),
			GetDescrtion = lists:nth(1,string:tokens(lists:nth(2, SplitingPart), ",")),
			utils:clean_quotes(GetDescrtion);
		_ ->
			error_logger:error_msg("Gist with ID: " ++ integer_to_list(Id) ++ " " ++
								   "obtaining error")
	end.

%%
%% @spec get_gist_pull_url(Id) -> ["Url"]
%% @doc - Get gist pull url
%% @type - Id = Int
%% @type - Url = String
%%
get_gist_pull_url(Id) ->
	TryGetGist = get_gist(Id),
	case TryGetGist of
		{ok, "200", _, Content} ->
			%
			% find git_pull_url tag
			%
	    	Rstr = string:rstr(Content, "\"git_pull_url\""),
			%
			% Get sub_string to "git_pull_url"
			%
			SubString = string:sub_string(Content, Rstr),
			%
			% Split by ":"
			%
			Tokens = string:tokens(SubString, ":"),
			%
			% Form path
			%
			Path = lists:nth(1,string:tokens(lists:nth(3, Tokens), ",")),
			utils:clean_quotes(lists:nth(2, Tokens) ++ ":" ++ Path);
		_ ->
			error_logger:error_msg("Gist with ID: " ++ integer_to_list(Id) ++ " " ++
								   "obtaining error")
	end.	