%% @author Jonathan Freedman <jonafree@gmail.com>
%% @copyright (c) 2012 ExactTarget

%% @doc Monitors a subset of source/beam files for changes.
%%
%% The hotbeam component of magicbeam will monitor a subset of modules depending on configuration.
%% It will extract the source and beam file locations and pay attemtion to when these files are modified.
%% If a file is modified and the requisite configuration option is set then hotbeam will recompile
%% and or reload the file.
%% @end

-module(hotbeam).
-behaviour(gen_server).
-author('jonafree@gmail.com').

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

%% @private
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec mod(Module :: atom()) -> ok
%% @doc Request the reload of a single module
%%
%% Module does not neccesarily have to be already known to hotbeam

mod(Module) when is_atom(Module) ->
    ok = gen_server:cast(?MODULE, {reload_mod, Module}).

%% @spec app(Application :: atom()) -> ok
%% @doc Request the reload of all modules in a single application
%%
%% Application must be configured and known to hotbeam
app(Application) when is_atom(Application) ->
    ok = gen_server:cast(?MODULE, {reload_app, Application}).
   
%% @spec all() -> ok
%% @doc Request the reloading of all known modules
all() ->
    ok = gen_server:cast(?MODULE, reload_all).

%% @spec info() -> [{Key:: atom(), Value :: term()}]
%% @doc Display general hotbeam diagnostic info
info() ->
    {ok, Result} = gen_server:call(?MODULE, info),
    Result.

%% @spec info(Mod::atom()) -> [{Key::atom(), Value::term()}]
%% @doc Display hotbeam diagnostic info on a specific beam
info(Mod) when is_atom(Mod) ->
    {ok, Result} = gen_server:call(?MODULE, {info, Mod}),
    Result.

%% @spec lazyload(Mod::atom()) -> ok
%% @doc Attempt to compile and reload given module
lazyload(Mod) -> ok = gen_server:cast(?MODULE, {lazyload, Mod}).

%% @spec rehash() -> ok
%% @doc Rehash configuration from application environment
rehash() -> ok = gen_server:cast(?MODULE, rehash).

%% @private
init([]) ->
    false = process_flag(trap_exit, true),
    {ok, p_timer(p_rehash(#hotbeam_state{}))}.

%% @private
handle_call(info, _From, State) ->
    {reply, {ok, p_info(State)}, State};
handle_call({info, Mod}, _From, State) ->
    {reply, {ok, p_info(Mod, State)}, State};
handle_call(_, _From, State) -> {noreply, State}.

%% @private
handle_cast({reload_app, Application}, State) ->
    {noreply, hotload_app(Application, State)};
handle_cast({reload_mod, Module}, State) ->
    {noreply, hotload_mod(Module, State)};
handle_cast(reload_all, State) ->
    {noreply, hotload(State#hotbeam_state.apps, State)};
handle_cast({lazyload, Mod}, State) ->
    ?info("manually recompiling ~p", [Mod]),
    {noreply, case lists:keysearch(Mod, #hotbeam.mod, State#hotbeam_state.beams) of
                  {value, #hotbeam{} = HB} ->
                      p_lazy_load(HB, State);
                  false ->
                      reload_mod(Mod, State)
              end};
handle_cast(rehash, State) ->
    {noreply, p_rehash(State)};
handle_cast(_, State) -> {noreply, State}.

%% @private
handle_info(loop, State) -> {noreply, p_timer(p_rescan(State))};
handle_info(_, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> p_cleanup().

%% @private
code_change(_Old, State, _Extra) -> {ok, State}.

%% @private
p_cleanup() ->
    TmpDir = p_temp_dir(),
    ok = case file:del_dir(TmpDir) of
             ok -> ok;
             {error, _} = E -> ok = ?warn("unable to remove tmpdir ~p - ~p", [TmpDir, E])
         end.

%% @private
p_rehash(State) ->
    p_timer(State#hotbeam_state{
      enable = ?HOTBEAM_ENABLED,
      src = ?HOTBEAM_COMPILE,
      apps = ?HOTBEAM_APPS
     }).

%% @private
p_timer(#hotbeam_state{enable = true, tref = undefined} = State) ->
    {ok, Tref} = timer:send_after(?HOTBEAM_LOOP, self(), loop),
    State#hotbeam_state{tref = Tref};
p_timer(#hotbeam_state{enable = false, tref = undefined} = State) ->
    State;
p_timer(#hotbeam_state{tref = Tref} = State) ->
    {ok, cancel} = timer:cancel(Tref),
    p_timer(State#hotbeam_state{tref = undefined}).

%% @private
hotload(Applications, State) -> hotload1(Applications, State).
hotload1([], State) -> State;
hotload1([App | Apps], State) ->
    hotload1(Apps, hotload_app(App, State)).

%% @private
hotload_app(App, State) when is_atom(App) ->
    case application:get_key(App, modules) of
        {ok, Mods} ->
            NewState = lists:foldl(fun(M, S) -> reload_mod(M, S) end, State, Mods),
            ok = ?info("Reloaded Application ~s", [App]),
            NewState
    end.

%% @private
hotload_mod(Mod, State) when is_atom(Mod) ->
    case code:which(Mod) of
        Path when is_list(Path) -> reload_mod(Mod, State);
        non_existing ->
            ok = ?warn("attempted to reload non-existent module ~p", [Mod]),
            State
    end.

%% @private
reload_mod(Mod, #hotbeam_state{beams = Beams} = State) when is_atom(Mod) ->
    HF = fun(M, B) -> case lists:keyfind(M, #hotbeam.mod, B) of false -> undefined; #hotbeam{} = HB -> HB end end,
    case {loaded, HF(Mod, Beams)} of
        {loaded, undefined} ->
            S0 = #hotbeam_state{beams = NewBeams} = p_rescan_fun(Mod, State),
            case {new, HF(Mod, NewBeams)} of
                {new, undefined} ->
                    ?info("unable to add ~p", [Mod]),
                    State;
                {new, #hotbeam{} = HB} ->
                    reload_mod(HB, S0)
            end;
        {loaded, #hotbeam{} = HB} ->
            reload_mod(HB, State)
    end;
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
                    ?info("reloaded ~p", [Mod]),
                    State#hotbeam_state{hotload_count = HC + 1, beams = lists:keystore(Mod, #hotbeam.mod, Beams, HB)};
                {load, {error, nofile}} ->
                    State#hotbeam_state{beams = lists:keydelete(Mod, #hotbeam.mod, Beams)};
                {load, E = {error, _R}} ->
                    ok = ?error("error reloading module ~p: ~p", [Mod, E]),
                    State
            end
    end.

%% @private
%% @doc Checks module for callback function and if available will attempt to run
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

%% @private
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

%% @private
p_sourcefile(Mod) when is_atom(Mod) -> p_sourcefile(Mod, Mod:module_info(compile)).
p_sourcefile(Mod, Compile) when is_list(Compile) ->
    SMod = atom_to_list(Mod),
    {value, {source, FileName}} = lists:keysearch(source, 1, Compile),
    case string:right(SMod, 4)  of
        "_dtl" ->
            case filename:extension(FileName) of
                ".dtl" ->
                    FileName;
                L when is_list(L) ->
                    FileName ++ "/templates/" ++ string:left(SMod, length(SMod) - 4) ++ ".dtl"
            end;
        L when is_list(L) ->
            FileName
    end.

%% @private
%% @doc Handles the reloading of modules
%%
%% This fun is called as part of the reloading loop. It checks file times and compares against the last
%% known times for each module. If appropriate it will recompile and/or reload a module.
p_rescan_fun(Mod, #hotbeam_state{enable = Enable, src = SrcEnable, beams=Beams} = State) ->
    case {mod, code:is_loaded(Mod), lists:keysearch(Mod, #hotbeam.mod, Beams)} of
        {mod, false, _} ->
            Mod:module_info(), % force loading of module when in interactive mode
            case code:is_loaded(Mod) of
                false ->
                    State;
                _ ->
                    p_rescan_fun(Mod, State)
            end;
        {mod, _, false} ->
            Beam = code:which(Mod),
            Src = p_sourcefile(Mod),
            State#hotbeam_state{beams = lists:keystore(Mod, #hotbeam.mod, Beams, #hotbeam{
                                                                                mod = Mod,
                                                                            src=Src,
                                                                            beam=Beam,
                                                                            beam_time = p_filetime(Beam),
                                                                            src_time = p_filetime(Src)})};
        {mod, _, {value, #hotbeam{beam=B,src=S,beam_time=BT,src_time=ST} = HB}} when Enable == true ->
            SrcTime = p_filetime(S),
            BeamTime = p_filetime(B),
            case {reload, SrcEnable, if ST == error -> false ; true -> ST /= SrcTime end, if BT == error -> false ; true -> BT /= BeamTime end} of
                {reload, true, true, _} ->
                    ?info("recompiling ~p", [Mod]),
                    p_lazy_load(HB#hotbeam{last = ?enow(), src_time = SrcTime}, State);
                {reload, _, false, true} ->
                    reload_mod(HB#hotbeam{last = ?enow(), beam_time = BeamTime}, State);
                {reload, _, _, _} -> State#hotbeam_state{beams = lists:keystore(Mod, #hotbeam.mod, Beams, HB#hotbeam{last = ?enow()})}
            end;
        {mod, _, _} when Enable == false -> State
    end.

%% @private
p_rescan_mods(AppMods, #hotbeam_state{} = State) ->
    lists:foldl(fun p_rescan_fun/2, State, AppMods).

%% @private
p_rescan(#hotbeam_state{apps = Apps} = State) ->
    Then = now(),
    NewState = p_rescan(Apps, State),
    ScanTime = round(timer:now_diff(now(), Then) / 1000),
    NewState#hotbeam_state{scantime = ScanTime}.

%% @private
p_rescan([], State) -> State;
p_rescan([App | Apps], #hotbeam_state{} = State) ->
    case application:get_key(App, modules) of
        undefined -> p_rescan(Apps, State);
        {ok, AppMods} -> p_rescan(Apps, p_rescan_mods(AppMods, State))
    end.

%% @private
p_filetime(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime = MTime}} ->
            case calendar:local_time_to_universal_time_dst(MTime) of
		[UTC] -> UTC;
                [_, UTC] -> UTC
            end;
        {error, enoent} -> error;
	{error, enotdir} -> error
    end.

%% @private
p_lazy_load(#hotbeam{mod = Mod} = HB, #hotbeam_state{compile_count = CC, beams = Beams} = State) ->
    CResp = compile(Mod),
    {_RResp, NewState} = if CResp == ok -> {ok, reload_mod(Mod, State#hotbeam_state{compile_count = CC + 1})} ; true -> {error, State} end,
    NewState#hotbeam_state{beams = lists:keystore(Mod, #hotbeam.mod, Beams, HB)}.

%% @spec compile(CompMod::atom()) -> ok | error
%% @doc Discovers compile options and attempts magic
%%
%% The module attributes are checked for include directories and source file location. Given
%% this information will attempt to recompile given module.
compile(CompMod) when is_atom(CompMod) ->
    Compile = CompMod:module_info(compile),
    FileName = p_sourcefile(CompMod, Compile),
    case filename:extension(FileName) of
        ".erl" -> 
            compile_beam(CompMod, FileName, Compile);
        ".dtl" ->
            compile_dtl(CompMod, FileName, Compile)
    end.

compile_dtl(CompMod, FileName, Compile) ->
    TmpDir = p_temp_dir(),
    OutDir = filename:dirname(code:which(CompMod)),
    Opts = [
            verbose,
            {out_dir, TmpDir},
            {compiler_options, [
                                {source, FileName}
                               ]}
           ],
    case erlydtl:compile(FileName, CompMod, Opts) of
        ok ->
            magicbeam_srv:event({hotbeam, compile, CompMod}),
            ok = move_beam(TmpDir, CompMod, OutDir);
        error ->
            ?info("Failed to compile dtl ~p", [FileName]),
            error
    end.                                

compile_beam(CompMod, FileName, Compile) ->
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
    NewDefines = lists:filter(fun({d, _}) -> true ; ({d, _, _}) -> true; (_) -> false end, CompileOpts),
    Resp = case compile:file(File, [return, debug_info, {outdir, TmpDir}] ++ IncludeDirs ++ NewDefines) of
               {ok, CompMod, _Warnings} ->
                   magicbeam_srv:event({hotbeam, compile, CompMod}),
                   ok = move_beam(TmpDir, CompMod, OutDir);
               {error, Errors, Warnings} when is_list(Errors), is_list(Warnings) ->
                   ?info("Failed to compile ~p with ~p errors, ~p warnings", [File, length(Errors), length(Warnings)]),
                   error
           end,
    file:set_cwd(Pwd),
    Resp.

%% @private
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

%% @private
p_info(#hotbeam_state{apps=Apps, beams=B, enable = E, src = S, hotload_count = HC, compile_count = CC}) ->
    [
     {enable, E},
     {src, S},
     {apps, Apps},
     {tracked, length(B)},
     {count, [{hotload, HC},{compile, CC}]}
    ].

%% @private
p_info(Mod, #hotbeam_state{beams = Beams}) ->
    case lists:keysearch(Mod, #hotbeam.mod, Beams) of
        false -> [];
        {value, #hotbeam{beam = B, src = S, beam_time = BT, src_time = ST, last = L}} ->
            Now = ?enow(),
            [
             {beam, B},
             {src, S},
             {time, [{src, if is_integer(ST) -> Now - ST; true -> ST end}, {beam, if is_integer(BT) -> Now - BT; true -> BT end}]},
             {last, Now - L}
            ]
    end.
