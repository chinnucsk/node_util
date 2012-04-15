%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc handles the upgrade of a node.
%%%
%%% @end
%%% Created :  2 Feb 2012 by Andreas Hasselberg <andreas.hasselberg@gmail.com>

-module(node_upgrade).

-export([do/1]).

do(File) ->
    try
	{ok, Version} = release_handler:unpack_release(File),
	{ok, _OldVersion, _Desc} = release_handler:install_release(Version),
	ok = release_handler:make_permanent(Version),
	io:format("Upgrade done.")
    catch
	Class:Exception ->
	    io:format("Upgrade failed ~p:~p~nStack:~p", [Class, Exception, erlang:get_stacktrace()])
    end.

