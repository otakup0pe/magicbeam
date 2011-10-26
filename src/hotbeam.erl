-module(hotbeam).
-behaviour(gen_server).

-export([
    start_link/0,
    mod/1,
    app/1,
    all/0,
    info/0,
    info/1,
    compile/1,
    lazyload/1,
    rehash/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("magicbeam.hrl").

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

mod(Module) when is_atom(Module) -> 
    ok = gen_server:cast(?MODULE, {reload_mod, Module}).

app(Application) when is_atom(Application) ->
    ok = gen_server:cast(?MODULE, {reload_app, Application}).

all() ->
    ok = gen_server:cast(?MODULE, reload_all).

info() ->
    {ok, Result} = gen_server:call(?MODULE, info),
    Result.

info(Mod) when is_atom(Mod) -> 
    {ok, Result} = gen_server:call(?MODULE, {info, Mod}),
    Result.

lazyload(Mod) -> ok = gen_server:cast(?MODULE, {lazyload, Mod}).

rehash() -> ok = gen_server:cast(?MODULE, rehash).

init([]) -> 
    false = process_flag(trap_exit, true),
    {ok, p_timer(p_rehash(#hotbeam_state{}))}.

handle_call(info, _From, State) ->
    {reply, {ok, p_info(State)}, State};
handle_call({info, Mod}, _From, State) ->
    {reply, {ok, p_info(Mod, State)}, State};
handle_call(_, _From, State) -> {noreply, State}.

handle_cast({reload_app, Application}, State) ->
    {noreply, hotload_app(Application, State)};
handle_cast({reload_mod, Module}, State) ->
    {noreply, hotload_mod(Module, State)};
handle_cast(reload_all, State) ->
    {noreply, hotload(State#hotbeam_state.apps, State)};
handle_cast({lazyload, Mod}, State) ->
    {noreply, case lists:keysearch(Mod, #hotbeam.mod, State#hotbeam_state.beams) of
        {value, #hotbeam{} = HB} -> 
            ?info("manually recompiling ~p", [Mod]),
            p_lazy_load(HB, State);
        false -> State
    end};
handle_cast(rehash, State) ->
    {noreply, p_rehash(State)};
handle_cast(_, State) -> {noreply, State}.

handle_info(loop, State) -> {noreply, p_timer(p_rescan(State))};
handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> p_cleanup().

code_change(_Old, State, _Extra) -> {ok, State}.

p_cleanup() ->
    TmpDir = p_temp_dir(),
    ok = case file:del_dir(TmpDir) of
        ok -> ok;
        {error, _} = E -> ok = ?warn("unable to remove tmpdir ~p - ~p", [TmpDir, E])
    end.

p_rehash(State) ->
    State#hotbeam_state{
        enable = ?HOTBEAM_ENABLED,
        src = ?HOTBEAM_COMPILE,
        apps = ?HOTBEAM_APPS
    }.

p_timer(#hotbeam_state{tref = undefined} = State) ->
    {ok, Tref} = timer:send_after(?HOTBEAM_LOOP, self(), loop),
    State#hotbeam_state{tref = Tref};
p_timer(#hotbeam_state{tref = Tref} = State) ->
    {ok, cancel} = timer:cancel(Tref),
    p_timer(State#hotbeam_state{tref = undefined}).

hotload(Applications, State) -> hotload1(Applications, State).
hotload1([], State) -> State;
hotload1([App | Apps], State) ->
    hotload1(Apps, hotload_app(App, State)).

hotload_app(App, State) when is_atom(App) ->
    case application:get_key(App, modules) of
	    {ok, Mods} ->
	        NewState = lists:foldl(fun(M, S) -> reload_mod(M, S) end, State, Mods),
	        ok = ?info("Reloaded Application ~s", [App]),
	        NewState
    end.

hotload_mod(Mod, State) when is_atom(Mod) ->
    case code:which(Mod) of
    	Path when is_list(Path) -> reload_mod(Mod, State);
    	non_existing -> 
    	    ok = ?warn("attempted to reload non-existent module ~p", [Mod]),
    	    State
    end.

reload_mod(Mod, #hotbeam_state{beams = Beams} = State) when is_atom(Mod) -> reload_mod(lists:keyfind(Mod, #hotbeam.mod, Beams), State);
reload_mod(false, State) -> State;
reload_mod({value, #hotbeam{} = HB}, State) -> reload_mod(HB, State);
reload_mod(#hotbeam{mod = Mod} = HB, #hotbeam_state{hotload_count = HC, beams = Beams} = State) when is_atom(Mod)->
    case {sticky, code:is_sticky(Mod)} of
        {sticky, true} -> State;
        {sticky, false} ->
            code:purge(Mod),
            code:soft_purge(Mod),
            case {load, code:load_file(Mod)} of
                {load, {module, Mod}} ->
                    ok = hotload_callback(Mod),
                    magicbeam_srv:event({hotbeam, reload, Mod}),
                    State#hotbeam_state{hotload_count = HC + 1, beams = lists:keystore(Mod, #hotbeam.mod, Beams, HB#hotbeam{beam_time = ?enow()})};
                {load, {error, E}} -> 
                    ok = ?error("error reloading module ~p: ~p", [Mod, E]),
                    State
            end
    end.

hotload_callback(Mod) ->
    case {exported, erlang:function_exported(Mod, hotload, 0)} of
        {exported, false} -> ok;
        {exported, true} ->
            case {callback, catch Mod:hotload()} of
                {callback, ok} -> ok;
                {callback, _R} ->
                    ok = ?warn("unable to execute ~p:hotload()", [Mod])
            end
    end.

move_beam(TmpDir, Mod, Dir) ->
    BeamName = atom_to_list(Mod) ++ ".beam",
    SourceBeam = TmpDir ++ BeamName,
    DestDir = Dir ++ "/" ++ BeamName,
    case file:copy(SourceBeam, DestDir) of
        {ok, _Bytes} -> ok = file:delete(SourceBeam);
        {error, _E} = Error -> 
            ?warn("unable to copy ~p to ~p - ~p", [SourceBeam, DestDir, Error]),
            error
    end.

p_sourcefile(Mod) when is_atom(Mod) -> p_sourcefile(Mod:module_info(compile));
p_sourcefile(Compile) when is_list(Compile) ->
    {value, {source, FileName}} = lists:keysearch(source, 1, Compile),
    FileName.

p_rescan_fun(Mod, #hotbeam_state{enable = Enable, src = SrcEnable, beams=Beams} = State) ->
    Mod:module_info(), %force loading of module when in interactive mode
    case {mod, code:is_loaded(Mod), lists:keysearch(Mod, #hotbeam.mod, Beams)} of
        {mod, false, _} -> State;
        {mod, _, false} -> 
            Beam = code:which(Mod),
            Src= p_sourcefile(Mod),
            State#hotbeam_state{beams = lists:keystore(Mod, #hotbeam.mod, Beams, #hotbeam{
                mod = Mod,
                src=Src,
                beam=Beam,
                beam_time = p_filetime(Beam),
                src_time = p_filetime(Src)})};
        {mod, _, {value, #hotbeam{beam=B,src=S,beam_time=BT,src_time=ST} = HB}} when Enable == true ->
            SrcTime = p_filetime(S),
            BeamTime = p_filetime(B),
            case {reload, SrcEnable, ST < SrcTime, BT < BeamTime} of
                {reload, true, true, _} -> 
                    ?info("recompiling ~p", [Mod]),
                    p_lazy_load(HB#hotbeam{last = ?enow()}, State);
                {reload, _, false, true} -> 
                    ?info("reloading ~p", [Mod]),
                    reload_mod(HB#hotbeam{last = ?enow()}, State);
                {reload, _, _, _} -> State#hotbeam_state{beams = lists:keystore(Mod, #hotbeam.mod, Beams, HB#hotbeam{last = ?enow()})}
            end;
        {mod, _, _} when Enable == false -> State
    end.

p_rescan_mods(AppMods, #hotbeam_state{} = State) ->
    lists:foldl(fun p_rescan_fun/2, State, AppMods).

p_rescan(#hotbeam_state{apps = Apps} = State) -> 
    Then = now(),
    NewState = p_rescan(Apps, State),
    ScanTime = round(timer:now_diff(now(), Then) / 1000),
    NewState#hotbeam_state{scantime = ScanTime}.

p_rescan([], State) -> State;
p_rescan([App | Apps], #hotbeam_state{} = State) ->
    case application:get_key(App, modules) of
        undefined -> p_rescan(Apps, State);
        {ok, AppMods} -> p_rescan(Apps, p_rescan_mods(AppMods, State))
    end.

p_filetime(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime = MTime}} -> 
            [MTimeUTC] = calendar:local_time_to_universal_time_dst(MTime),
            calendar:datetime_to_gregorian_seconds(MTimeUTC);
        {error, enoent} -> error
    end.

p_lazy_load(#hotbeam{mod = Mod} = HB, #hotbeam_state{compile_count = CC, beams = Beams} = State) ->
    CResp = compile(Mod),
    {_RResp, NewState} = if CResp == ok -> {ok, reload_mod(Mod, State#hotbeam_state{compile_count = CC + 1})} ; true -> {error, State} end,
    NewState#hotbeam_state{beams = lists:keystore(Mod, #hotbeam.mod, Beams, HB#hotbeam{src_time = ?enow()})}.

compile(CompMod) when is_atom(CompMod) ->
    Compile = CompMod:module_info(compile),
    FileName = p_sourcefile(Compile),
    File = filename:rootname(FileName, ".erl"),
    {value, {options, CompileOpts}} = lists:keysearch(options, 1, Compile),
    OutDir = filename:dirname(code:which(CompMod)),
    IncludeDirs1 = lists:filter(fun({i, _Dir}) -> true; (_) -> false end, CompileOpts),
    MyInclude = filename:dirname(filename:dirname(FileName)) ++ "/include",
    IncludeDirs = case lists:member({i, MyInclude}, IncludeDirs1) of
        true -> IncludeDirs1;
        false -> IncludeDirs1 ++ [{i, MyInclude}]
    end,
    Pwd = file:get_cwd(),
    CompileDir = case lists:keysearch(cwd, 1, Compile) of
        {value, {cwd, Dir}} -> Dir;
        false -> Pwd
    end, 
    file:set_cwd(CompileDir),
    TmpDir = p_temp_dir(),
    Resp = case compile:file(File, [return, debug_info, {outdir, TmpDir}] ++ IncludeDirs) of
        {ok, CompMod, _Warnings} ->
            magicbeam_srv:event({hotbeam, compile, CompMod}),
            ok = move_beam(TmpDir, CompMod, OutDir);
        {error, Errors, Warnings} when is_list(Errors), is_list(Warnings) ->
            ?info("Failed to compile ~p with ~p errors, ~p warnings", [File, length(Errors), length(Warnings)]),
            error
    end,
    file:set_cwd(Pwd),
    Resp.

p_temp_dir() ->
    TmpDir = "/tmp/" ++ atom_to_list(node()) ++ "_hotload/",
    ok = case {check, file:read_file_info(TmpDir)} of
        {check, {ok, _Info}} -> ok;
        {check, {error, enoent}} ->
            case {create, file:make_dir(TmpDir)} of
                {create, ok} -> ok
            end
    end,
    TmpDir.

p_info(#hotbeam_state{apps=Apps, beams=B, enable = E, src = S, hotload_count = HC, compile_count = CC}) ->
    [
        {enable, E},
        {src, S},
        {apps, Apps},
        {tracked, length(B)},
        {count, [{hotload, HC},{compile, CC}]}
    ].

p_info(Mod, #hotbeam_state{beams = Beams}) ->
    case lists:keysearch(Mod, #hotbeam.mod, Beams) of
        false -> [];
        {value, #hotbeam{beam = B, src = S, beam_time = BT, src_time = ST, last = L}} ->
            Now = ?enow(),
            [
                {beam, B},
                {src, S},
                {time, [{src, Now - ST}, {beam, Now - BT}]},
                {last, Now - L}
            ]
    end.
