-module(thunderbeam).
-behaviour(gen_server).

-include("thunderbeam.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, kill/0, info/0, rehash/0]).

-record(state, {enabled, tref=undefined, killed=0, base, variable}).

kill() -> gen_server:cast(?MODULE, killer).
info() -> gen_server:call(?MODULE, info).
rehash() -> gen_server:cast(?MODULE, rehash).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    {ok, p_rehash(#state{})}.

handle_call(info, _From, State) -> {reply, p_info(State), State};
?handle_call_tail.
handle_cast(killer, State) -> {noreply, p_kill(State)};
handle_cast(rehash, State) -> {noreply, p_rehash(State)};
?handle_cast_tail.
handle_info(ding, State) -> {noreply, p_rehash_timer(p_kill(State))};
?handle_info_tail.

terminate(_Reason, _State) -> 
    ok.

code_change(_Old, State, _Extra) -> {ok, State}.

p_info(#state{enabled = Enabled, killed=Killed}) ->
    [
        {enabled, Enabled},
        {killed, Killed}
    ].

p_rehash(State) ->
    S0 = State#state{
        base=?KILLER_FREQUENCY_BASE,
        variable=?KILLER_FREQUENCY_VARIABLE,
        enabled=?KILLER_ENABLE
    },
    p_rehash_timer(S0).

p_rehash_timer(#state{tref=TRef} = State) when is_tuple(TRef) ->
    timer:cancel(TRef),
    p_rehash_timer(State#state{tref=undefined});
p_rehash_timer(#state{tref=undefined, enabled = true, base=Base, variable=Variable} = State) ->
    Time = (Base * random:uniform(Variable)),
    ?info("Killing next process in ~p seconds", [Time]),
    {ok, TRef} = timer:send_after(Time * 1000, self(), ding),
    State#state{tref=TRef};
p_rehash_timer(State) -> State.

p_kill(#state{enabled=false} = State) ->
    ?warn("kill requested when disabled", []),
    p_real_kill(State);
p_kill(State) -> p_real_kill(State).

p_real_kill(State) -> p_real_kill(State, erlang:processes()).
p_real_kill(State, ProcessList) -> p_real_kill(State, ProcessList, random:uniform(length(ProcessList))).
p_real_kill(State, [PID| PIDS], Count) when ( length(PIDS) + 1 ) == Count ->
    p_kill1(State, PID);
p_real_kill(State, [_PID | PIDS], Count) -> p_real_kill(State, PIDS, Count).

p_kill1(State, PID) -> p_kill1(State, PID, erlang:process_info(PID, [registered_name])).
p_kill1(State, PID, [{registered_name, []}]) -> p_kill2(State, PID, "N/A");
p_kill1(State, PID, [{registered_name, Name}]) when is_atom(Name) ->
    case lists:member(Name, ?KILLER_IMMUNE_PROC) of
        false -> p_kill2(State, PID, Name);
        true -> 
            ?warn("Not killing immune process ~p", [Name]),
            State
    end.

p_kill2(State, PID, Name) -> p_kill2(application:get_application(PID), State, PID, Name).
p_kill2(undefined, State, PID, Name) -> p_kill3(State, PID, Name);
p_kill2({ok, Application}, State, PID, Name) ->
    case lists:member(Application, ?KILLER_IMMUNE_APP) of
        false -> p_kill3(State, PID, Name);
        true ->
            ?warn("Not killing proc ~p from immune app ~p", [Name, Application]),
            State
    end.

p_kill3(#state{killed = Killed} = State, PID, Name) ->
    p_kill_warn(PID, Name),
    case process_info(PID, trap_exit) of
        {trap_exit, true} -> PID ! suicide;
        {trap_exit, false} -> erlang:exit(PID, nomura_test_killer)
    end,
    State#state{killed = Killed + 1}.

p_kill_warn(PID, Name) -> p_kill_warn(PID, Name, erlang:process_info(PID, [current_function, messages, trap_exit, links])).
p_kill_warn(PID, Name, Info) ->
    {M, F, Ar} = proplists:get_value(current_function, Info, "N/A"),
    MC = length(proplists:get_value(messages, Info, [])),
    Trap = proplists:get_value(trap_exit, Info, false),
    LC = length(proplists:get_value(links, Info, [])),
    ok = ?warn("Killing ~p(~p) while in ~p:~p/~p (MC:~p L:~p T:~p)", [PID, Name, M, F, Ar, MC, LC, Trap]).
