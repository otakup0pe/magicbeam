%% @author Jonathan Freedman <jonafree@gmail.com>
%% @copyright (c) 2012 ExactTarget

%% @doc Flagrantly flaunts stability

-module(thunderbeam).
-behaviour(gen_server).
-author('jonafree@gmail.com').

-ifdef(TEST).
-compile(export_all).
-endif.

-include("magicbeam.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, kill/0, info/0, rehash/0, enabled/1]).

%% @spec kill() -> ok
%% @doc Kills a single, random, process
kill() -> gen_server:cast(?MODULE, killer).

%% @spec info() -> [{Key::atom(), Value::term()}]
%% @doc Returns general thunderbeam diagnostic information
info() -> gen_server:call(?MODULE, info).

%% @private
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec enabled(true | false) -> ok
%% @doc Selectively enable or disables thunderbeam
enabled(B) when B == true; B == false ->
    ok = application:set_env(magicbeam, thunderbeam_enabled, B),
    rehash().

%% @spec rehash() -> ok
%% @doc Rehash configuration from application environment
rehash() -> gen_server:cast(?MODULE, rehash).

%% @private
init([]) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    {ok, p_rehash(#thunderbeam_state{})}.

%% @private
handle_call(info, _From, State) -> {reply, p_info(State), State}.

%% @private
handle_cast(killer, State) -> {noreply, p_kill(State)};
handle_cast(rehash, State) -> {noreply, p_rehash(State)}.

%% @private
handle_info(loop, State) -> {noreply, p_timer(p_kill(State))};
handle_info(_, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_Old, State, _Extra) -> {ok, State}.

%% @private
p_rehash(State) ->
    p_timer(State#thunderbeam_state{
              enabled = ?THUNDERBEAM_ENABLED,
              base = ?THUNDERBEAM_WAIT_BASE,
              variable = ?THUNDERBEAM_WAIT_VARIABLE,
              immune_proc = ?THUNDERBEAM_IMMUNE_PROC,
              immune_app = ?THUNDERBEAM_IMMUNE_APP,
              force_apps = ?THUNDERBEAM_FORCE_APPS
             }).

%% @private
p_info(#thunderbeam_state{enabled=Enabled, killed=Killed}) ->
    [
     {enabled, Enabled},
     {killed, Killed}
    ].

%% @private
p_timer(#thunderbeam_state{tref=TRef, enabled = true} = State) when is_tuple(TRef) ->
    timer:cancel(TRef),
    p_timer(State#thunderbeam_state{tref=undefined});
p_timer(#thunderbeam_state{tref=undefined, enabled = true, base=Base, variable=Variable} = State) ->
    Time = (Base * random:uniform(Variable)),
    ?info("Killing next process in ~p seconds", [Time]),
    {ok, TRef} = timer:send_after(Time * 1000, self(), loop),
    State#thunderbeam_state{tref=TRef};
p_timer(#thunderbeam_state{tref=TRef, enabled = false} = State) when is_tuple(TRef) ->
    timer:cancel(TRef),
    State#thunderbeam_state{tref=undefined};
p_timer(State) -> State.

%% @private
p_kill(State) -> p_kill(State, ?THUNDERBEAM_KILL_ATTEMPTS).
%% @private
p_kill(State, 0) ->
    ?warn("unable to find process to kill", []),
    State;
p_kill(State, Attempts) -> p_kill(State, Attempts, erlang:processes()).
p_kill(State, Attempts, ProcessList) -> p_kill(State, Attempts, ProcessList, random:uniform(length(ProcessList))).
p_kill(State, Attempts, [PID| PIDS], Count) when ( length(PIDS) + 1 ) == Count ->
    p_kill1(State, Attempts, PID);
p_kill(State, Attempts, [_PID | PIDS], Count) -> p_kill(State, Attempts, PIDS, Count).

%% @private
p_kill1(State, Attempts, PID) when is_pid(PID) -> p_kill1(State, Attempts, PID, erlang:process_info(PID, [registered_name])).
%% @private
p_kill1(State, Attempts, PID, [{registered_name, []}]) -> p_kill2(State, Attempts, PID, "N/A");
p_kill1(#thunderbeam_state{immune_proc = IP} = State, Attempts, PID, [{registered_name, Name}]) when is_atom(Name) ->
    case lists:member(Name, IP) of
        false -> p_kill2(State, Attempts, PID, Name);
        true ->
            p_kill(State, Attempts - 1)
    end.

%% @private
p_kill2(State, Attempts, PID, Name) -> p_kill2(application:get_application(PID), Attempts, State, PID, Name).
%% @private
p_kill2(undefined, _Attempts, State, PID, Name) -> p_kill3(State, PID, Name);
p_kill2({ok, Application}, Attempts, #thunderbeam_state{immune_app = IA, force_apps = FA} = State, PID, Name) ->
    case {lists:member(Application, FA), lists:member(Application, IA)} of
	{true, true} -> p_kill3(State, PID, Name);
        {false, false} -> p_kill3(State, PID, Name);
        _ -> p_kill(State, Attempts - 1)
    end.

%% @private
p_kill3(#thunderbeam_state{killed = Killed} = State, PID, Name) ->
    p_kill_warn(PID, Name),
    case process_info(PID, trap_exit) of
        {trap_exit, true} -> p_kill_trap_exit(PID, Name, State);
        {trap_exit, false} ->
            erlang:exit(PID, thunderbeam),
            p_kill_event(PID, Name),
            State#thunderbeam_state{killed = Killed + 1}
    end.

%% @private
p_kill_trap_exit(PID, Name, #thunderbeam_state{killed = Killed} = State) ->
    PID ! seppuku,
    case {is_process_alive(PID), ?THUNDERBEAM_FORCE_KILL} of
        {true, false} ->
            ?warn("unable to force-kill ~p as disallowed by configuration", [Name]),
            State#thunderbeam_state{killed = Killed};
        {true, true} ->
            erlang:exit(PID, kill),
            p_kill_event(PID, Name),
            State#thunderbeam_state{killed = Killed + 1};
        {false, _} ->
            p_kill_event(PID, Name),
            State#thunderbeam_state{killed = Killed + 1}
    end.

%% @private
p_kill_warn(PID, Name) -> p_kill_warn(PID, Name, erlang:process_info(PID, [current_function, messages, trap_exit, links])).
%% @private
p_kill_warn(PID, Name, Info) ->
    {M, F, Ar} = proplists:get_value(current_function, Info, "N/A"),
    MC = length(proplists:get_value(messages, Info, [])),
    Trap = proplists:get_value(trap_exit, Info, false),
    LC = length(proplists:get_value(links, Info, [])),
    ok = ?warn("Killing ~p(~p) while in ~p:~p/~p (MC:~p L:~p T:~p)", [PID, Name, M, F, Ar, MC, LC, Trap]).

%% @private
p_kill_event(PID, Name) ->
    magicbeam_srv:event({thunderbeam, kill, {PID, Name}}).
