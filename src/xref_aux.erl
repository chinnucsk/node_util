%%%-------------------------------------------------------------------
%%% File    : xref_aux.erl
%%% Author  : Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% Description :
%%%
%%% Created : 14 Apr 2009 by Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%%-------------------------------------------------------------------
-module(xref_aux).

-export([start/0]).

-ignore_xref({start, 0}).

dirs() ->
    [Dir || Dir <- code:get_path(), "apps" == string:substr(Dir, 1, 4)].

start() ->
    do(dirs()), init:stop().

do(Dirs) ->
    xref:start(?MODULE),
    try
        setup(Dirs),
        analyse()
    catch E:R ->
            io:format("Error ~p, Reason ~p~nStack ~p",
                      [E, R, erlang:get_stacktrace()]),
            error
    end,
    xref:stop(?MODULE).

setup(Dirs) ->
    xref:set_library_path(?MODULE, code:get_path()),
    [xref:add_directory(?MODULE, Dir) || Dir <- Dirs].

analyse() ->
    Ignores = lists:append([ignores(Mod)
                            || {Mod, _} <- xref:info(?MODULE, modules)]),
    Res =
        lists:zf(
          fun(Analyse) -> do_analyse(Analyse, Ignores) end,
          analysis()),
    if Res =/= [] ->
            io:format("Ignoring modules~n~p~n", [ignore_modules()]),
            [io:format("~s~n", [R]) || R <- Res];
       true ->
            ok
    end.

do_analyse(Analyse, Ignores) ->
    {ok, Result0} = xref:analyse(?MODULE, Analyse),
    Result = [Res || Res <- Result0,
                     filter_ignores(Res, Ignores),
                     filter_ignore_modules(Res)],
    out_if_res(Analyse, Result).

out_if_res(_Analyse, []) -> false;
out_if_res(Analyse, Result) ->
    {true, io_lib:format("Errors in ~p:~n~p~n", [Analyse, Result])}.

analysis() ->
    [undefined_function_calls, undefined_functions,
     locals_not_used,
     exports_not_used,
     deprecated_function_calls].

ignores(M) ->
    Attrs     = mks(attributes, M:module_info()),
    Ignore    = mks(ignore_xref, Attrs),
    Callbacks = [B:behaviour_info(callbacks) ++ maybe_maybe_callbacks(B) ++
                     case B of
                         gen_server -> [{start_link, 0}];
                         _ -> []
                     end ||
                    B <- mks(behaviour, Attrs)],
    [{M, F, A} || {F, A} <- Ignore ++ lists:flatten(Callbacks)].

mks(Key, List) ->
    lists:flatten([Value || {K, Value} <- List, K == Key]).

filter_ignores({MFA1, MFA2}, Ignores) ->
    filter_ignores(MFA1, Ignores) andalso filter_ignores(MFA2, Ignores);
filter_ignores({M, F, A} = MFA, Ignores)
  when is_atom(M), is_atom(F), is_integer(A) ->
    not (lists:member(MFA, Ignores) orelse is_eunit(F, A)).

is_eunit(test, 0) -> true;
is_eunit(F, 0) ->
    case lists:reverse(atom_to_list(F)) of
        "tset_"  ++ _ -> true;
        "_tset_" ++ _ -> true;
        _ -> false
    end;
is_eunit(_F, _A) -> false.

filter_ignore_modules({MFA1, MFA2}) ->
    filter_ignore_modules(MFA1) andalso filter_ignore_modules(MFA2);
filter_ignore_modules({M, _F, _A}) ->
    not lists:member(M, ignore_modules()).

maybe_maybe_callbacks(B) ->
    try B:behaviour_info(maybe_callbacks) of
        undefined -> [];
        L when is_list(L) -> L
    catch
        error:undef ->
            [];
        error:function_clause ->
            []
    end.

ignore_modules() ->
    [erlang, init, %% weird
     user_default %% manually called
    ].
