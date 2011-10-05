-module(hotbeam).
-behaviour(gen_server).

-export([
    start_link/0,
    mod/1,
    app/1,
    all/0,
    info/0,
    compile/1,
    lazyload/1,
    rehash/0
]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("magicbeam.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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

lazyload(Mod) -> gen_server:cast(?MODULE, {lazyload, Mod}).
rehash() -> gen_server:cast(?MODULE, rehash).

init([]) -> 
    false = process_flag(trap_exit, true),
    {ok, p_rehash(#hotbeam_state{})}.

p_rehash(State) ->
    p_lazy_toggle(State#hotbeam_state{
        enabled = ?HOTBEAM_ENABLED,
        apps = ?HOTBEAM_APPS
    }).

handle_call(info, _From, State) ->
    {reply, {ok, p_info(State)}, State};
handle_call(_, _From, State) -> {noreply, State}.

handle_cast({reload_app, Application}, State) ->
    hotload_app(Application),
    {noreply, State};
handle_cast({reload_mod, Module}, State) ->
    hotload_mod(Module),
    {noreply, State};
handle_cast(reload_all, State) ->
    hotload(State#hotbeam_state.apps),
    {noreply, State};
handle_cast({lazyload, Mod}, State) ->
    {noreply, p_lazy_load(Mod, State)};
handle_cast(rehash, State) ->
    {noreply, p_rehash(State)};
handle_cast(_, State) -> {noreply, State}.

handle_info(lazy_loop, State) -> {noreply, p_lazy_loop(p_rescan(State))};
handle_info(_, State) -> {noreply, State}.

terminate(Reason, _State) when Reason == normal; Reason == shutdown -> p_cleanup();
terminate(_Reason, State) ->
    nomura_angel:set(?MODULE, State),
    p_cleanup().

p_cleanup() ->
    TmpDir = p_temp_dir(),
    ok = case file:del_dir(TmpDir) of
        ok -> ok;
        {error, _} = E -> ok = ?warn("unable to remove tmpdir ~p - ~p", [TmpDir, E])
    end.

code_change(_Old, State, _Extra) -> {ok, State}.

hotload(Applications) ->
    ?debug("hotloading applications ~p", [Applications]),
    hotload1(Applications).
hotload1([]) -> ok;
hotload1([App | Apps]) ->
    ok = hotload_app(App),
    hotload1(Apps).

hotload_app(App) when is_atom(App) ->
    case application:get_key(App, modules) of
	    undefined -> ok;
	    {ok, Mods} ->
	        lists:foreach(fun(M) -> reload_mod(M) end, Mods),
	        ok = ?info("Reloaded Application ~s", [App])
    end.

hotload_mod(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
    	Path when is_list(Path) -> reload_mod(Mod);
    	non_existing -> 
    	    ok = ?warn("attempted to reload non-existent module ~p", [Mod])
    end.

reload_mod(Mod) when is_atom(Mod)->
    case {sticky, code:is_sticky(Mod)} of
        {sticky, true} -> ?debug("not reloading sticky module: ~p", [Mod]);
        {sticky, false} ->
            code:purge(Mod),
            code:soft_purge(Mod),
            case {load, code:load_file(Mod)} of
                {load, {module, Mod}} -> 
                    nomura_stats:bump(lazy_hotload),
                    ok = ?debug("reloaded module ~p", [Mod]),
                    hotload_callback(Mod);
                {load, {error, E}} -> ok = ?error("error reloading module ~p: ~p", [Mod, E])
            end
    end.

hotload_callback(Mod) ->
    case {exported, erlang:function_exported(Mod, hotload, 0)} of
        {exported, false} -> ok;
        {exported, true} ->
            case {callback, catch Mod:hotload()} of
                {callback, ok} -> ok = ?debug("executed ~p:hotload()", [Mod]);
                {callback, R} -> 
                    ?debug("non-ok return from ~p:hotload() - ~p", [Mod, R]),
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

p_lazy_toggle(#hotbeam_state{enabled = false, tref = undefined} = State) -> State;
p_lazy_toggle(#hotbeam_state{enabled = false} = State) -> 
    ?info("lazy loading disabled", []),
    p_disable_timer(State#hotbeam_state{modtimes = []});
p_lazy_toggle(#hotbeam_state{enabled = true, tref = undefined} = State) -> 
    ?warn("lazy loading enabled", []),
    p_enable_timer(p_rescan(State#hotbeam_state{modtimes = []}));
p_lazy_toggle(State) -> State.

p_enable_timer(#hotbeam_state{tref=undefined} = State) ->
    {ok, Tref} = timer:send_after(5, self(), lazy_loop),
    State#hotbeam_state{tref = Tref};
p_enable_timer(#hotbeam_state{tref=Tref} = State) when is_tuple(Tref) ->
    p_enable_timer(p_disable_timer(State)).

p_disable_timer(#hotbeam_state{tref=undefined} = State) -> State;
p_disable_timer(#hotbeam_state{tref=Tref} = State) when is_tuple(Tref) ->
    {ok, cancel} = timer:cancel(Tref),
    State#hotbeam_state{tref = undefined}.

p_rescan_fun(ModTimes) ->
    fun(Mod) ->
        case {code:is_loaded(Mod), lists:keysearch(Mod, 1, ModTimes)} of
            {false, _} -> ok;
            {_, false} -> {Mod, 0, p_sourcefile(Mod)};
            {_, {value, {Mod, _Time, _File}}} -> ok            
        end
    end.

p_rescan_mods(AppMods, ModTimes) ->
    NewModTimes = lists:map(p_rescan_fun(ModTimes), AppMods),
    lists:filter(fun
        ({_M, _T, _F}) -> true ; 
        (ok) -> false 
    end, NewModTimes).

p_rescan(#hotbeam_state{apps = Apps} = State) -> 
    Then = now(),
    NewState = p_rescan(Apps, State),
    ScanTime = round(timer:now_diff(now(), Then) / 1000),
    ?debug("rescan took ~p ms", [ScanTime]),
    NewState.

p_rescan([], State) -> State;
p_rescan([App | Apps], #hotbeam_state{modtimes = ModTimes} = State) ->
    case application:get_key(App, modules) of
        undefined -> p_rescan(Apps, State);
        {ok, AppMods} ->
            ?debug("scanning ~p : ~p mods", [App, length(AppMods)]),
            p_rescan(Apps, State#hotbeam_state{modtimes = ModTimes ++ p_rescan_mods(AppMods, ModTimes)})
    end.

p_filetime(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime = MTime}} -> 
            [MTimeUTC] = calendar:local_time_to_universal_time_dst(MTime),
            calendar:datetime_to_gregorian_seconds(MTimeUTC);
        {error, enoent} -> error
    end.
 
p_lazy_loop(#hotbeam_state{modtimes = ModTimes} = State) -> p_lazy_loop(ModTimes, State).
p_lazy_loop([], State) -> p_enable_timer(State);
p_lazy_loop([{Mod, 0, _File} | ModTimes], State) ->
    p_lazy_loop(ModTimes, p_bump_modtime(Mod, State));
p_lazy_loop([{Mod, LTime, File} | ModTimes], State) ->
    case p_filetime(File) of
        error ->
            ?warn("problem with file ~p; removing", [File]),
            State#hotbeam_state{modtimes = lists:keydelete(File, 3, State#hotbeam_state.modtimes)};
        ModTime when is_integer(ModTime) andalso ModTime > LTime ->
            p_lazy_loop(ModTimes, p_lazy_load(Mod, State));
        ModTime when is_integer(ModTime) -> p_lazy_loop(ModTimes, State)
    end.

p_lazy_load(Mod, State) ->
    CResp = compile(Mod),
    RResp = if CResp == ok -> reload_mod(Mod) ; true -> error end,
    case {CResp, RResp} of
        {ok, ok} -> 
            ?info("lazy hotloaded ~p", [Mod]);
        _ -> ok
    end,
    p_bump_modtime(Mod, State).

p_bump_modtime(Mod, State) -> p_bump_modtime(Mod, State, ?enow()).
p_bump_modtime(Mod, #hotbeam_state{modtimes = ModTimes} = State, Time) ->
    case lists:keysearch(Mod, 1, ModTimes) of
        {value, {Mod, _OldTime, File}} -> State#hotbeam_state{modtimes = lists:keystore(Mod, 1, ModTimes, {Mod, Time, File})};
        false -> State
    end.

p_sourcefile(Mod) when is_atom(Mod) -> p_sourcefile(Mod:module_info(compile));
p_sourcefile(Compile) when is_list(Compile) ->
    {value, {source, FileName}} = lists:keysearch(source, 1, Compile),
    FileName.

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
        {ok, CompMod, Warnings} -> 
            WCount = if length(Warnings) > 0 -> io_lib:format(" with ~p warnings", [length(Warnings)]) ; true -> "" end,
            ?debug("Succesfully recompiled ~s~s", [File ++ ".erl", WCount]),
            ok = move_beam(TmpDir, CompMod, OutDir);
        {error, Errors, Warnings} when is_list(Errors), is_list(Warnings) ->
            ?info("Failed to compile ~p with ~p errors, ~p warnings", [File, length(Errors), length(Warnings)]),
            error
    end,
    file:set_cwd(Pwd),
    Resp.

p_temp_dir() ->
    Release = nomura_util:release_name(),
    TmpDir = "/tmp/" ++ atom_to_list(Release) ++ "_hotload/",
    ok = case {check, file:read_file_info(TmpDir)} of
        {check, {ok, _Info}} -> ok;
        {check, {error, enoent}} ->
            case {create, file:make_dir(TmpDir)} of
                {create, ok} -> ok
            end
    end,
    TmpDir.

p_info(#hotbeam_state{tref=TRef, apps=Apps, modtimes=ModTimes}) ->
    [
        {lazyload, if TRef == undefined -> false; true -> true end},
        {apps, Apps},
        {mods, length(ModTimes)}
    ].
