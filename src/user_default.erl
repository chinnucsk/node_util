%%% File    : user_default.erl
%%% Author  : Andreas Hasselberg <>
%%% Description : 
%%% Created :  1 Mar 2009 by Andreas Hasselberg <>

-module(user_default).

-export([c/1, c/2, i/0, i/1, i/2, i/3, ni/0, ci/0, ci/3, cni/0, bt/1, bt/3]).
-export([q/0, halt/0]).
-export([pi/0, pi/1, pi/2, pi2/0]).
-export([p/1]).
-export([pid/1,pid/2]).
-export([e/2]).
-export([lm/0, mm/0]).
-export([src/1, beam/1, cl/3]).
-export([ok/1,ok2/1,ok3/1]).
-export([parse_transform/2]).
-export([help/0,dbgtc/1, dbgon/1, dbgon/2,
	 dbgadd/1, dbgadd/2, dbgdel/1, dbgdel/2, dbgoff/0]).
-export([rbugon/1,rbugon/2,rbugoff/0]).
-export([d/2]).
-export([sig/1,sig/2,long/1]).

-import(lists, [filter/2, foreach/2, flatmap/2]).
-import(io, [format/1]).


%%%
%%% See:
%%% http://www.erlang.org/pipermail/erlang-questions/2006-September/022750.html
%%%
help() ->
  shell_default:help(),
  format(help_text()),
  true.

help_text() ->
    "** user extended commands **\n"
	"dbgtc(File)   -- use dbg:trace_client() to read data from File\n"
	"dbgon(M)      -- enable dbg tracer on all funs in module M\n"
	"dbgon(M,Fun)  -- enable dbg tracer for module M and function F\n"
	"dbgon(M,File) -- enable dbg tracer for module M and log to File\n"
	"dbgadd(M)     -- enable call tracer for module M\n"
	"dbgadd(M,F)   -- enable call tracer for function M:F\n"
	"dbgdel(M)     -- disable call tracer for module M\n"
	"dbgdel(M,F)   -- disable call tracer for function M:F\n"
	"dbgoff()      -- disable dbg tracer (calls dbg:stop/0)\n"
	"rbugon(M)     -- enable redbug tracer on all funs in module M\n"
	"rbugon(M,F)   -- enable redbug tracer for function M:F\n"
	"rbugoff(M)    -- disable redbug tracer\n"
	"lm()          -- load all changed modules\n"
	"cl(M,F,A)     -- call unexported function\n"
	"nl()          -- load all changed modules on all known nodes\n"
	"mm()          -- list modified modules\n"
    .


pid(I2,I3)		      -> pid({I2,I3}).
pid({I1,I2,I3})		      -> c:pid(I1,I2,I3);
pid({I2,I3})		      -> pid({0,I2,I3});
pid(Pid)  when is_pid(Pid)    -> Pid;
pid(Atom) when is_atom(Atom)  -> whereis(Atom);
pid(I2)   when is_integer(I2) -> pid({0,I2,0});
pid(Str)  when hd(Str)==$<    -> list_to_pid(Str);
pid(Str)  when is_list(Str)   -> pid("<"++Str++">").

q()    -> warn_quit_msg().
halt() -> warn_quit_msg().

%% element(N,Thing)
e(N,T) when is_list(T) -> lists:nth(N,T);
e(N,T) when is_tuple(T) -> element(N,T).

warn_quit_msg() ->
    "Kill the system? Use init:stop() (or C-c C-c for remote node).".

dbgtc(File) ->
    Fun = fun({trace,_,call,{M,F,A}}, _) ->
                 io:format("call: ~w:~w~w~n", [M,F,A]);
             ({trace,_,return_from,{M,F,A},R}, _) ->
                 io:format("retn: ~w:~w/~w -> ~w~n", [M,F,A,R]);
             (A,B) ->
                 io:format("~w: ~w~n", [A,B])
          end,
    dbg:trace_client(file, File, {Fun, []}).

dbgon(Module) ->
    case dbg:tracer() of
    {ok,_} ->
       dbg:p(all,call),
       dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
       ok;
    Else ->
       Else
    end.

dbgon(Module, Fun) when is_atom(Fun) ->
    {ok,_} = dbg:tracer(),
    dbg:p(all,call),
    dbg:tpl(Module, Fun, [{'_',[],[{return_trace}]}]),
    ok;

dbgon(Module, File) when is_list(File) ->
    {ok,_} = dbg:tracer(file, dbg:trace_port(file, File)),
    dbg:p(all,call),
    dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
    ok.

dbgadd(Module) ->
    dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
    ok.

dbgadd(Module, Fun) ->
    dbg:tpl(Module, Fun, [{'_',[],[{return_trace}]}]),
    ok.

dbgdel(Module) ->
    dbg:ctpl(Module),
    ok.

dbgdel(Module, Fun) ->
    dbg:ctpl(Module, Fun),
    ok.

dbgoff() ->
    dbg:stop().



rbugon(Module) when is_atom(Module) ->
    code:ensure_loaded(Module),
    redbug:start(10000000, 10000, [{Module}]).

rbugon(Module, Func) when is_atom(Module), is_atom(Func) ->
    code:ensure_loaded(Module),
    redbug:start(10000000, 10000, [{Module, Func}]).

rbugoff() ->
    redbug:stop().


c(M) ->
    c(M, []).

c(M, Opts) ->
    case shellc(M, Opts) of
	error -> 
	    try src(M) of
		S -> shellc(S, Opts ++ [{outdir,filename:dirname(beam(M))}])
	    catch error: E -> E
	    end;
	O -> O
    end.

shellc(M, Opts) ->
    shell_default:c(M, Opts++[debug_info]).

p(Term) ->
    io:format("~p\n", [Term]).

%% Good ol' i() but includes zooombie support
i() -> i1(processes()).
ni() -> i1(all_procs()).


i(X,Y,Z) -> i({X,Y,Z}).
i(X,Y) -> i({X,Y}).
i(X) ->
  case pid(X) of
    P when is_pid(P) -> pinfo(P);
    P -> P
  end.

%% i(Pid) when is_pid(Pid) -> pinfo(Pid);
%% i(Name) when is_atom(Name) ->
%%     case whereis(Name) of
%% 	undefined -> undefined;
%% 	Pid -> i(Pid)
%%     end.

%%i(X,Y,Z) -> pinfo(c:pid(X,Y,Z)).


%% If you like the new one
ci() ->
    c:i().

ci(X,Y,Z) ->
    c:i(X,Y,Z).

cni() ->
    c:ni().


%% Code moified from c.erl
i1(Ps) ->
    Alive = filter(fun palive/1, Ps),
    i2(Alive),
    case filter(fun pzombie/1, Ps) of
	[] ->
	    ok;
	Zombies ->
	    %% Zombies is not the same as Ps-Alive, since the remote
	    %% process that fetched Ps is included among Alive, but has
	    %% exited (for ni/0).
	    io:format("\nDead processes:\n"),
	    i2(Zombies)
    end.

i2(Ps) ->
    iformat("Pid", "Initial Call", "Current Function", "Reds", "Msgs"),
    {R,M} = lists:foldl(fun display_info/2, {0,0}, Ps),
    iformat("Total", "", "", io_lib:write(R), io_lib:write(M)).

palive(Pid) ->
    case pinfo(Pid, status) of
	undefined         -> false;
	{status, exiting} -> false;
	_                 -> true
    end.

pzombie(Pid) ->
    case pinfo(Pid, status) of
	undefined         -> false;
	{status, exiting} -> true;
	_                 -> false
    end.

pinfo(Pid) ->
  N = node(Pid),
  case 
    case N =:= node() of
	 true -> [];
	 false-> [{node,N}]
       end ++
    case rpc:call(N,erlang,process_info,[Pid]) of
      L when is_list(L) -> L;
      _ -> []
    end of
    [] -> [];
    I -> [{pid,Pid}|I]
  end.

pinfo(Pid, Item) ->
    case is_alive() of
	true -> rpc:call(node(Pid), erlang, process_info, [Pid, Item]);
	false -> process_info(Pid, Item)
    end.

all_procs() ->
    case is_alive() of
	true -> flatmap(fun (N) -> rpc:call(N, erlang, processes, []) end,
			[node() | nodes()]);
	false -> processes()
    end.

display_info(Pid, {R,M}) ->
    case pinfo(Pid) of
	[] ->
	    {R, M};
	Info ->
	    Call = initial_call(Info),
	    Curr = fetch(current_function, Info),
	    Reds = fetch(reductions, Info),
	    LM = fetch(message_queue_len, Info),
	    iformat(io_lib:write(Pid),
		    mfa_string(Call),
		    mfa_string(Curr),
		    io_lib:write(Reds),
		    io_lib:write(LM)),
	    {R+Reds, M+LM}
    end.

%% We can do some assumptions about the initial call.
%% If the initial call is proc_lib:init_p/5 we can find more information
%% by calling the function proc_lib:translate_initial_call/1.
initial_call(Info)  ->
    case fetch(initial_call, Info) of
	{proc_lib, init_p, 5} ->
	    proc_lib:translate_initial_call(Info);
	ICall ->
	    ICall
    end.

mfa_string({M, F, A}) ->
    io_lib:format("~w:~w/~w", [M, F, A]);
mfa_string(X) ->
    io_lib:write(X).

fetch(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
	{value, {_, Val}} -> Val;
	false -> 0
    end.

iformat(A1, A2, A3, A4, A5) ->
    io:format("~-12s ~-23s ~-23s ~12s ~4s\n", [A1,A2,A3,A4,A5]).


%% Port info
%% I don't really know which info is most relevent, so I included
%% both pi() and pi2().
pi() ->
    piformat("Id", "Name", "Connected", "Initial Call", "Current Function"),
    do_pi(fun(Info) ->
		  Id = fetch(id, Info),
		  Name = fetch(name, Info),
		  case fetch(connected, Info) of
		      Pid when is_pid(Pid) ->
			  {ICall, Curr} =
			      case pinfo(Pid) of
				  [] ->
				      {[], []};
				  ProcInfo ->
				      {initial_call(ProcInfo),
				       fetch(current_function, ProcInfo)}
			      end,
			  piformat(io_lib:write(Id), 
				   Name,
				   io_lib:write(Pid),
				   mfa_string(ICall),
				   mfa_string(Curr));
		      Port when is_port(Port) ->
			  piformat(io_lib:write(Id), 
				   Name,
				   io_lib:write(Port),
				   "","")
		  end
	  end).
	     
piformat(A1, A2, A3, A4, A5) ->
    io:format("~-6s ~-10s ~-12s ~-23s ~-23s\n", [A1,A2,A3,A4,A5]).

pi2() ->
    pi2format("Id", "Name", "Connected", "Recv", "Sent"),
    do_pi(fun(Info) ->
		  Id = fetch(id, Info),
		  Name = fetch(name, Info),
		  Pid = fetch(connected, Info),
		  Recv = fetch(input, Info),
		  Sent = fetch(output, Info),
		  pi2format(io_lib:write(Id), 
			   Name,
			   io_lib:write(Pid),
			   io_lib:write(Recv),
			   io_lib:write(Sent))
	  end).

pi2format(A1, A2, A3, A4, A5) ->
    io:format("~-6s ~-20s ~-12s ~-10s ~-10s\n", [A1,A2,A3,A4,A5]).

do_pi(Print) ->
    foreach(
      fun(P) ->
	      case erlang:port_info(P) of
		  undefined ->
		      ok;
		  Info ->
		      Print(Info)
	      end
      end, erlang:ports()).


pi(Id) ->
    pi_l(erlang:ports(), Id).

pi_l([P | Ps], Id) ->
    case erlang:port_info(P, id) of
	{id, Id} ->
	    erlang:port_info(P);
	_ ->
	    pi_l(Ps, Id)
    end;
pi_l([], _Id) ->
    undefined.


pi(X,Y) ->
    PStr = lists:flatten(io_lib:format("#Port<~w.~w>", [X,Y])),
    pi_l2(erlang:ports(), PStr).

pi_l2([P | Ps], PStr) ->
    case lists:flatten(io_lib:format("~w", [P])) of
	PStr ->
	    erlang:port_info(P);
	_ ->
	    pi_l2(Ps, PStr)
    end;
pi_l2([], _PStr) ->
    undefined.

%% Doesn't do process_display, which means it can be used when
%% remotely connecting to a node.
bt(Pid) when is_pid(Pid) ->
    case pinfo(Pid, backtrace) of
	{backtrace, Bin} ->
	    io:format("~s\n", [binary_to_list(Bin)]);
	_ ->
	    undefined
    end;
bt(Name) when is_atom(Name) ->
    case whereis(Name) of
	undefined -> undefined;
	Pid -> bt(Pid)
    end.


bt(X,Y,Z) ->
    bt(c:pid(X,Y,Z)).

%% Code provided by Vladimir Sekissov <svg@surnet.ru>
mm() ->
  modified_modules().

lm() ->
    [c:l(M) || M <- mm()].

modified_modules() ->
  [M || {M, _} <-  code:all_loaded(), module_modified(M) == true].

module_modified(Module) ->
  case code:is_loaded(Module) of
    {file, preloaded} ->
      false;
    {file, Path} ->
      CompileOpts = proplists:get_value(compile, Module:module_info()),
      CompileTime = proplists:get_value(time, CompileOpts),
      Src = proplists:get_value(source, CompileOpts),
      module_modified(Path, CompileTime, Src);
    _ ->
      false
  end.


module_modified(Path, PrevCompileTime, PrevSrc) ->
  case find_module_file(Path) of
    false ->
      false;
    ModPath ->
      case beam_lib:chunks(ModPath, ["CInf"]) of
        {ok, {_, [{_, CB}]}} ->
	      CompileOpts =  binary_to_term(CB),
	      CompileTime = proplists:get_value(time, CompileOpts),
	      Src = proplists:get_value(source, CompileOpts),
	      not (CompileTime == PrevCompileTime) and (Src == PrevSrc);
	  _ ->
	      false
      end
  end.

find_module_file(Path) ->
  case file:read_file_info(Path) of
    {ok, _} ->
      Path;
    _ ->
      %% may be the path was changed?
      case code:where_is_file(filename:basename(Path)) of
	non_existing ->
	  false;
	NewPath ->
	  NewPath
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Call local.
cl(M,F,A) ->
  if is_list(A) -> 
      try_fun(M,F,A);
     is_tuple(A) ->
      try_fun(M,F,tuple_to_list(A));
     true -> 
      try_fun(M,F,[A])
  end.


src(Module) ->
    proplists:get_value(source,
			proplists:get_value(compile,  
					    Module:module_info())).
beam(Module) ->
    code:which(Module).


try_fun(M,F,Args)->
  export(M,F,length(Args)),
  R = apply(M,'APPLY',Args),
  code:soft_purge(M),code:load_file(M),
  R.

export(M,F,A) ->
  {_,_,B}=compile:file(src(M),[{parse_transform,user_default},
			  {export_fun,{F,A}},binary]),
  code:soft_purge(M),
  code:load_binary(M,"",B).

parse_transform(A,B) ->
  case lists:keysearch(export_fun,1,B) of
    {value,{export_fun,{F,Arity}}} ->
      Last = element(2,lists:last(A)),
      add_fun('APPLY',Arity,
	      mk_fun(F,Arity,Last),A,Last+4);
    _ -> A
  end.


mk_fun(F,A,N) ->
  {function,N+1,'APPLY',A,
   [{clause,N+2,vars(A,N+2),[],
     [{call,N+3,{'fun',N+3,{function,F,A}},
       vars(A,N+3)}]}]}.

vars(0,_P) -> [];
vars(N,P) -> [{var,P,list_to_atom("V"++[$A+N])}|
	       vars(N-1,P)].

add_fun(Name,Arity,Body,Code,Eof) ->
  add_eof(
    add_exp(Code,Name,Arity),Body,Eof).

add_eof([{eof,_}],Body,Eof) -> [Body,{eof,Eof}];
add_eof([N|R],Body,Eof) -> [N|add_eof(R,Body,Eof)].

add_exp([{attribute,N,export,L}|R],Name,Arity) ->
  [{attribute,N,export,[{Name,Arity}|L]}|R];
add_exp([O|R],N,A) ->
 [O| add_exp(R,N,A)];
add_exp([],_,_) -> [].

ok(T) when is_tuple(T), size(T) >= 2, element(1, T) == ok -> element(2, T);
ok(E) -> erlang:error({not_ok, E}).

ok2(T) when is_tuple(T), size(T) >= 3, element(1, T) == ok -> element(3, T);
ok2(E) -> erlang:error({not_ok, E}).

ok3(T) when is_tuple(T), size(T) >= 4, element(1, T) == ok -> element(4, T);
ok3(E) -> erlang:error({not_ok, E}).

d(M, Row) ->
    %%shell_default:im(),
    shell_default:ii(M),
    shell_default:iaa([break]),
    shell_default:ib(M, Row).

sig(M) -> sig(M,'').
sig(M,F) when is_atom(M),is_atom(F) ->
    {error, {'NYI - R11B2', {otp_doc, sig, [M,F]}}}.

long(X) -> io:fwrite("~p~n",[X]).


