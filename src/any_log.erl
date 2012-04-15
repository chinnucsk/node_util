%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc
%%%
%%% @end
%%% Created : 28 Mar 2012 by Andreas Hasselberg <andreas.hasselberg@gmail.com>

-module(any_log).

-include("../include/any_log.hrl").

-define(APP, node_util).

-export([conf/1, log/2]).

log(Severity, String) ->
    case app_util:get_env(?APP, logger, io) of
	log4erl -> log4erl:log(Severity, String);
	io      -> io:format("~p LOG:~n~s~n~n", [Severity, String])
    end.

conf(App) ->
    filelib:ensure_dir("logs/"), %% This directory is needed by log4erl
    {ok, ConfFile} = application:get_env(App, log4erl_conf),
    ConfigFile = filename:join([code:priv_dir(App), ConfFile]),
    ok = log4erl:conf(ConfigFile),
    log4erl:error_logger_handler(),
    ?log(info, "log4erl conf set ~p", [ConfFile]).
