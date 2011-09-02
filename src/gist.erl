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
-export([get_created_time/1]).
-export([is_public/1]).
-export([get_content_size/1]).

% gist star
-export([gist_star/3]).
-export([gist_unstar/3]).

% gist comments
-export([gist_edit_comment/4]).
-export([gist_comment/4]).
-export([gist_delete_comment/3]).
-export([get_comments_count/1]).

% fork gist
-export([fork_gist/3]).

%% create | delete | edit gist
-export([create_gist/6]).
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
%% @spec get_content_size(Id) -> Size
%% @doc - Get gist content size
%% @type - Id = Int()
%% @type - Size = Int()
%%
get_content_size(Id) ->
	GetGist = get_gist(Id),
	case GetGist of
		{ok, "200", _, Content} ->
			Rstr = string:rstr(Content, "\"size\""),
			SubString = string:sub_string(Content, Rstr),
			Tokens = string:tokens(SubString, ":"),
			list_to_integer(lists:nth(1, string:tokens(lists:nth(2, Tokens), ",")));
		_ ->
			error_logger:error_msg("Gist with ID: " ++ integer_to_list(Id) ++ " " ++
			  				       "obtaining error")
	end.

%%
%% @spec get_gist_content(Id) -> Content
%% @doc - Get gist content
%% @type - Id = Int()
%% @type - Content = String()
%%
get_gist_content(Id) ->
	GetGist = get_gist(Id),
	case GetGist of
		{ok, "200", _, Content} ->
			FlatContent = lists:flatten(Content),
			Size = get_content_size(Id),
			Rstr1 = string:rstr(Content, "\"content\""),
			string:sub_string(FlatContent, Rstr1 + 11, Rstr1 + 11 + Size);
		_ ->
			error_logger:error_msg("Gist with ID: " ++ integer_to_list(Id) ++ " " ++
								   "obtaining error")
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
%% @spec create_gist(UserName, Password, Description, Public, File, Content) -> ok
%% @doc  - Create new gist
%% @type - UserName = String()
%% @type - Password = String()
%% @type - Description = String()
%% @type - Public = Boolean()
%% @type - File = String()
%% @type - Content = String()
%% @type -  ok = atom()
%%
create_gist(UserName, Password, Description, Public, File, Content) ->
	github:init(),
	MakeMessage = messages:make_create_gist_message(Description, Public, File, Content),
	ibrowse:send_req(?GISTS, [], post, MakeMessage,
				  [{basic_auth, {UserName, Password}},{stream_to, self()}, 
	    		   {ssl_options, [{verify,verify_none}, {depth, 3}]}]),
	ok.

%%
%% @spec gist_star(Id, UserName, Password) -> ok
%% @doc - Star gist
%% @type - Id = Int()
%% @type - UserName = String()
%% @type - Password = String()
%% @type - ok = atom()
%%
gist_star(Id, UserName, Password) ->
	github:init(),
	ibrowse:send_req(?GIST ++ integer_to_list(Id) ++ "/star", [], put, [],
		 		    [{basic_auth, {UserName, Password}},{stream_to, self()}, 
					{ssl_options, [{verify, 0}, {depth, 3}]}]),
	ok.

%%
%% @spec gist_unstar(Id, UserName, Password) -> ok
%% @doc - Unstar gist
%% @type - Id = Int()
%% @type - UserName = String()
%% @type - Password = String()
%% @type - ok = atom()
%%
gist_unstar(Id, UserName, Password) ->
	github:init(),
	ibrowse:send_req(?GIST ++ integer_to_list(Id) ++ "/star", [], delete, [],
		 		    [{basic_auth, {UserName, Password}},{stream_to, self()}, 
					{ssl_options, [{verify, 0}, {depth, 3}]}]),
	ok.

%%
%% @spec fork_gist(Id, UserName, Password) -> ok
%% @doc - fork gist by id
%% @type - Id = Int()
%% @type - UserName = String()
%% @type - Password = String()
%% @type - ok = atom()
%%
fork_gist(Id, UserName, Password) ->
	github:init(),
	ibrowse:send_req(?GIST ++ integer_to_list(Id) ++ "/fork", [], post, [],
		 		    [{basic_auth, {UserName, Password}},{stream_to, self()}, 
					{ssl_options, [{verify, 0}, {depth, 3}]}]),
	ok.

%%==============================================
%%             Github Gist comments
%%==============================================
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

%%
%% @spec gist_edit_comment(Id, Username, Password, NewComment) -> ok
%% @doc  - Edit comment with id - Id
%% @type - Id = Int()
%% @type - UserName = String()
%% @type - Password = String()
%% @type - NewComment = String()
%% @type -  ok = atom()
%%
gist_edit_comment(Id, Username, Password, NewComment) ->
    github:init(),
    EditComment = messages:make_gist_comment_message(NewComment),
	ibrowse:send_req(?GIST ++ "comments/" ++ integer_to_list(Id), [], post, EditComment,
			  [{basic_auth, {Username, Password}},{stream_to, self()}, 
	   		   {ssl_options, [{verify,verify_none}, {depth, 3}]}]),
	ok.

%%
%% @spec gist_delete_comment(Id, Username, Password) -> ok
%% @doc  - Delete gist comment with id - Id
%% @type - Id = Int()
%% @type - UserName = String()
%% @type - Password = String()
%% @type -  ok = atom()
%%
gist_delete_comment(Id, Username, Password) ->
	github:init(),
	ibrowse:send_req(?GIST ++ "comments/" ++ integer_to_list(Id), [], delete, [],
				  [{basic_auth, {Username, Password}},{stream_to, self()}, 
	    		   {ssl_options, [{verify,verify_none}, {depth, 3}]}]),
	ok.
