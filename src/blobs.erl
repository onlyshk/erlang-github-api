%%
%%Copyright (c) <2011>, Kuleshov Alexander <kuleshovmail@gmail.com>
%%All rights reserved.
%%

-module(blobs).

-export([get_blob_encoding/5]).
-export([get_blob_content/5]).

-include("define.hrl").

%%
%% @spec get_blob_content(UserName, Password, User, Repo, Sha) -> Content
%% @doc - Get blob content
%% @type - UserName = String()
%% @type - Password = String()
%% @type - User = String()
%% @type - Repo = String()
%% @type - Sha = String()
%% @type - Content = [String()]
%%
get_blob_content(UserName, Password, User, Repo, Sha) ->
	github:init(),
	Response = ibrowse:send_req(?REPOS  ++ User ++ "/" ++ Repo ++ "/git/blobs/" ++ Sha , [], get, [],
				               [{basic_auth, {UserName, Password}},{stream_to, self()}, 
	    		               {ssl_options, [{verify,verify_none}, {depth, 3}]}]),
	case Response of
		{ok, "200", _, Content} ->
			FlatContent = lists:flatten(Content),
			Rstr1 = string:rstr(FlatContent, "content"),
			Rstr2 = string:rstr(FlatContent, "encoding"),
			Tokens = string:sub_string(FlatContent, Rstr1 + 12, Rstr2 - 2),
			string:tokens(Tokens, ",");
		_ ->
			error_logger:error_msg("Blob with " ++ Sha ++  " " ++ 
									   "obtaining error")
	end.
		
%%
%% @spec get_blob_encoding(UserName, Password, User, Repo, Sha) -> Content
%% @doc - Get blob encoding
%% @type - UserName = String()
%% @type - Password = String()
%% @type - User = String()
%% @type - Repo = String()
%% @type - Sha = String()
%% @type - Content = [String()]
%%
get_blob_encoding(UserName, Password, User, Repo, Sha) ->
	github:init(),
	Response = ibrowse:send_req(?REPOS  ++ User ++ "/" ++ Repo ++ "/git/blobs/" ++ Sha , [], get, [],
				               [{basic_auth, {UserName, Password}},{stream_to, self()}, 
	    		               {ssl_options, [{verify,verify_none}, {depth, 3}]}]),
	case Response of
		{ok, "200", _, Content} ->
            FlatContent = lists:flatten(Content),
            Rstr2 = string:rstr(FlatContent, "encoding"),
            Tokens = string:tokens(string:sub_string(FlatContent, Rstr2), ":"),
			utils:clean_quotes(string:tokens(lists:nth(2, Tokens), " "));
		_ ->
			error_logger:error_msg("Blob with " ++ Sha ++  " " ++ 
									   "obtaining error")
	end.