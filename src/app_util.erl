%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc
%%%
%%% @end
%%% Created :  2 Apr 2012 by Andreas Hasselberg <andreas.hasselberg@gmail.com>

-module(app_util).

-export([get_env/3, is_running/1]).

get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
	{ok, Val} -> Val;
	undefined ->
	    case is_running(App) of
		false -> Default;
		true  -> throw({missing_env, App, Key})
	    end
    end.

is_running(App) ->
    {running, Apps} = lists:keyfind(running, 1, application:info()),
    case lists:keysearch(App, 1, Apps) of
	{value, {App, _}} -> true;
	false -> false
    end.
