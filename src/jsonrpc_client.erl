%%% @author Andreas Hasselberg <andreas.hasselberg@gmail.com>
%%% @copyright (C) 2012, Andreas Hasselberg
%%% @doc
%%% A jsonrpc client, similar to the one in yaws, but not depending on yaws.
%%% @end
%%% Created :  2 Apr 2012 by Andreas Hasselberg <andreas.hasselberg@gmail.com>

-module(jsonrpc_client).

-export([call/3, call/4]).

call(URL, Method, Args) ->
    call(URL, Method, Args, []).

call(URL, Method, Args, Options) ->
    {ok, CallPayloadDeep} = encode_call_payload(Method, Args),
    CallPayload = lists:flatten(CallPayloadDeep),
    {ok, Response} = httpc:request(
		       post,
		       {URL,[{"Content-Length",length(CallPayload)}],
			"application/x-www-form-urlencoded",CallPayload},
		       Options, []),
    
    RespBody = if
		   (size(Response) == 2) or (size(Response) == 3) ->
		       element(size(Response), Response)
	       end,
    decode_call_payload(RespBody).
	
encode_call_payload(Method, Args) when is_list(Args) ->
    encode_call_payload(Method, {array, Args});
encode_call_payload(Method, Args) ->
    %% id makes sense when there are many requests in same
    %% communication channel and replies can come in random
    %% order here it can be changed to something less expensive
    ID = element(3, erlang:now()),
    Struct =  mochijson2:encode({struct, [{"jsonrpc", "2.0"},
					  {method, Method},
					  {params, Args},
					  {id, ID}]}),
    {ok, Struct}.

%%%
%%% decode response structure
%%%
decode_call_payload(JSonStr) ->
    JSON = mochijson2:decode(JSonStr),
    Result = struct:get_value(<<"result">>, JSON),
    Error = struct:get_value(<<"error">>, JSON),
    case Error of
        undefined ->
            {ok,{response,[Result]}}; % make it compliant with xmlrpc response
        Error ->
            {error, Error}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

call_test() ->
    meck:new(httpc),
    meck:expect(httpc, request,
		fun(_, _, _, _) ->
			{ok, {{"HTTP/1.1",200,"OK"}, [], "{\"result\":100}"}} end),
    ?assertEqual({ok, {response, [100]}},
		 call("url", "Method", [])),
    meck:validate(httpc),
    meck:unload(httpc).

encode_test() ->
    ?assertMatch({ok, _}, encode_call_payload("Method", [])).

decode_test() ->
    ?assertMatch({ok, {response, _}},
		 decode_call_payload("{\"hej\":100}")).

-endif. %% TEST
