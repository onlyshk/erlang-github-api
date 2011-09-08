%%
%%Copyright (c) <2011>, Kuleshov Alexander <kuleshovmail@gmail.com>
%%All rights reserved.
%%

-module(reference).

-export([create_ref/6]).

-include("define.hrl").

%%==============================================
%%             Git reference
%%==============================================

%%
%% @spec create_ref(UserName, Password, User, Repo, Ref, Sha) -> ok
%% @doc -  Create ref
%% @type - UserName = String()
%% @type - Password = String()
%% @type - User = String()
%% @type - Repo = String()
%% @type - Ref = String()
%% @type - Sha = String()
%% @type - ok = atom()
%%
create_ref(UserName, Password, User, Repo, Ref, Sha) ->
	github:init(),
	MakeMessage = messages:make_ref_message(Ref, Sha),
	ibrowse:send_req(?REPOS  ++ User ++ "/" ++ Repo ++ "/git/refs" , [], post, MakeMessage,
				     [{basic_auth, {UserName, Password}},{stream_to, self()}, 
	    		     {ssl_options, [{verify,verify_none}, {depth, 3}]}]),
	ok.
