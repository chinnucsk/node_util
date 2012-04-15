-module(node_util_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP, node_util).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_crone(),
    node_util_sup:start_link().

start_crone() ->
    Tasks = app_util:get_env(?APP, crone_tasks, []),
    crone:start(Tasks).

stop(_State) ->
    ok.
