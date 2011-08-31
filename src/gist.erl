%%
%%Copyright (c) <2011>, Kuleshov Alexander <kuleshovmail@gmail.com>
%%All rights reserved.
%%

-module(gist).

%%
%% Exported Functions
%%

%% gist
-export([get_gist_pull_url/1]).
-export([get_gist_content/1]).
-export([get_gist_description/1]).
-export([get_gist_push_url/1]).
-export([get_comments_count/1]).
-export([get_created_time/1]).
-export([is_public/1]).

-export([gist_comment/4]).

%% create | delete | edit gist
-export([delete_gist/3]).

%% user
-export([get_gist_user_login/1]).

-include("define.hrl").

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
%% @type - Id = Int()
%% @type - Description = String()
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
%% @type - Id = Int()
%% @type - Url = String()
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

%%
%% @spec get_gist_push_url(Id) -> ["Url"]
%% @doc - Get gist push url()
%% @type - Id = Int()
%% @type - Url = String()
%%
get_gist_push_url(Id) ->
	TryGetGist = get_gist(Id),
	case TryGetGist of
		{ok, "200", _, Content} ->
			%
			% find git_pull_url tag
			%
	    		Rstr = string:rstr(Content, "\"git_push_url\""),
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

%%
%% @spec get_comments_count(Id) -> CommentsCount
%% @doc - Get gist comments count()
%% @type - Id = Int()
%% @type - CommentsCount = Int()
%%
get_comments_count(Id) ->
	TryGetGist = get_gist(Id),
	case TryGetGist of
		{ok, "200", _, Content} ->
			%
			% find git_pull_url tag
			%
	    		Rstr = string:rstr(Content, "\"comments\""),
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
			CountWithGb = utils:clean_quotes(lists:nth(2, Tokens) ++ ":" ++ Path),
			Comments = lists:nth(1,string:tokens(lists:nth(1, CountWithGb), ",")),
		    {CommentsCount, _} = string:to_integer(Comments),
			CommentsCount;
		_ ->
			error_logger:error_msg("Gist with ID: " ++ integer_to_list(Id) ++ " " ++
								   "obtaining error")
	end.

%%
%% @spec get_created_time(Id) -> [{Year, Month, Day}, {Hour, Minute}]
%% @doc - Get created time of this gist
%% @type - Id = Int()
%% @type - Year = Int()
%% @type - Month = Int()
%% @type - Day = Int()
%% @type - Hour = Int()
%% @type - Minute = Int()
%%
get_created_time(Id) ->
	TryGetGist = get_gist(Id),
	case TryGetGist of
		{ok, "200", _, Content} ->
			%
			% find git_pull_url tag
			%
	    		Rstr = string:rstr(Content, "created_at"),
			ForksRstr = string:rstr(Content, "forks"),
			if
				ForksRstr > Rstr ->
					SubString = string:sub_string(Content, Rstr),
					%
					% Split by ":"
					%
					Tokens = string:tokens(SubString, ":"),
					%
					% Form path
					%
					Path = lists:nth(1,string:tokens(lists:nth(3, Tokens), ",")),
					%
					% Split by :
					%
					CreatedAt = utils:clean_quotes(lists:nth(2, Tokens) ++ ":" ++ Path),
					%
					% Split time in date and time
					%
					[Date, Time] = string:tokens(lists:nth(1,CreatedAt), "T"),
					%
					% Get Y:M:D
					%
					[TYear, TMonth, TDay] = string:tokens(Date, "-"),
					%
					% Get H:M
					%
					[THour, TMinute] = string:tokens(Time, ":"),
					%
					% Map values to Integer
					%
					IValue = lists:map(fun(X) -> 
											   list_to_integer(X) 
									   end, 
									   [TYear, TMonth, TDay, THour, TMinute]),
					%
					% Get data
					%
					Year = lists:nth(1, IValue),
					Month = lists:nth(2, IValue),
					Day = lists:nth(3, IValue),
					Hour = lists:nth(4, IValue),
					Minute = lists:nth(5, IValue),
					[{Year, Month, Day}, {Hour, Minute}];
				true ->
					error
			end;
		_ ->
			error_logger:error_msg("Gist with ID: " ++ integer_to_list(Id) ++ " " ++
								   "obtaining error")
	end.

%%
%% @spec is_public(Id) -> Public
%% @doc -  Gist public or not
%% @type - Id = Int()
%% @type - Public = atom() - true | false
%%
is_public(Id) ->
	TryGetGist = get_gist(Id),
	case TryGetGist of
		{ok, "200", _, Content} ->
			Rstr = string:rstr(Content, "public"),
			SubString = string:sub_string(Content, Rstr),
			Tokens = string:tokens(SubString, ":"),
			%
			% Form path
			%
			list_to_atom(lists:nth(1, string:tokens(lists:nth(2, Tokens), ",")));
		_ ->
			error_logger:error_msg("Gist with ID: " ++ integer_to_list(Id) ++ " " ++
								   "obtaining error")
	end.	

%%
%% @spec get_gist_user_login(Id) -> UserName || Anonymous
%% @doc -  Get author gist
%% @type - Id = Int()
%% @type - UserName = String()
%% @type - Anonymous = String()
%%
get_gist_user_login(Id) ->
	TryGetGist = get_gist(Id),
	case TryGetGist of
		{ok, "200", _, Content} ->
			Rstr = string:rstr(Content, "users"),
			case Rstr of
				0 ->
					NewRstr = string:rstr(Content, "user"),
					SubString = string:sub_string(Content, NewRstr),
					Tokens = string:tokens(SubString, ","),
					Name = lists:last(string:tokens(lists:nth(1,Tokens), ":")),
					case Name of
						"null" ->
							"Anonymous";
						_ ->
							Name
					end;
            	_ ->
					SubString = string:sub_string(Content, Rstr),
					Tokens = string:tokens(SubString, ","),
				    
            		UserNameKV = string:tokens(lists:nth(1, Tokens), ":"),
					UserName = string:tokens(lists:nth(1, UserNameKV), "\""),
			        Login = lists:nth(2, string:tokens(lists:nth(1, UserName), "/")),
					case Login of
						"null" ->
							"Anonymous";
						_ ->
							 Login
					end
			end;
		_ ->
			error_logger:error_msg("Gist with ID: " ++ integer_to_list(Id) ++ " " ++
								   "obtaining error")
	end.	

%%
%% @spec delete_gist(Id, UserName, Password) -> ok
%% @doc  - Delete gist with id - ID
%% @type - Id = Int()
%% @type - UserName = String()
%% @type - Password = String()
%% @type - ok = atom()
%%
delete_gist(Id, UserName, Password) ->
	github:init(),
    ibrowse:send_req(?GIST ++ integer_to_list(Id), [], delete, [],
					  [{basic_auth, {UserName, Password}},{stream_to, self()}, 
					   {ssl_options, [{verify, 0}, {depth, 3}]}]),
	ok.

%%
%% @spec gist_comment(Id, Username, Password, Message) -> ok
%% @doc  - Set comment to gist with id - Id
%% @type - Id = Int()
%% @type - UserName = String()
%% @type - Password = String()
%% @type - Message = String()
%% @type -  ok = atom()
%%
gist_comment(Id, Username, Password, Message) ->
	github:init(),
	MakeMessage = messages:make_gist_comment_message(Message),
	ibrowse:send_req(?GIST ++ integer_to_list(Id) ++ "/comments", [], post, MakeMessage,
				  [{basic_auth, {Username, Password}},{stream_to, self()}, 
	    		   {ssl_options, [{verify,verify_none}, {depth, 3}]}]),
	ok.
	
