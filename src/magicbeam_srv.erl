-module(magicbeam_srv).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, cfgget/2]).

-record(state, {callback, state}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

cfgget(Key, Default) ->
    {ok, Val} = gen_server:call(?MODULE, {cfgget, Key, Default}),
    Val.

init([]) ->
    {ok, case magicbeam_util:appenv(callback, undefined) of
        undefined -> #state{};
        Mod when is_atom(Mod) ->
            false = erlang:process_flag(trap_exit, true),
            p_init_callback(#state{callback=Mod})
    end}.

p_init_callback(#state{callback=Mod} = State) ->
    true = code:is_loaded(Mod),
    case Mod:init() of
        {ok, CBState} -> State#state{state = CBState}
    end.

handle_call({cfgget, Key, Default}, _From, State) ->
    {Reply, NewState} = p_cfgget(Key, Default, State),
    {reply, {ok, Reply}, NewState};
handle_call(_, _From, State) -> {noreply, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

code_change(_Old, State, _Extra) -> {ok, State}.

terminate(_Reason, #state{callback = undefined}) -> ok;
terminate(Reason, #state{callback = Mod, state = CBState}) ->
    case catch Mod:terminate(Reason, CBState) of
        _ -> ok % so dirty
    end.

p_cfgget(Key, Default, #state{callback = undefined} = State) -> {magicbeam_util:appenv(Key, Default), State};
p_cfgget(Key, Default, #state{callback = Mod, state = CBState} = State) ->
    case Mod:cfgget(Key, Default) of
        undefined -> {magicbeam_util:appenv(Key, Default), State};
        {ok, Val, NewCBState} -> {Val, State#state{state = NewCBState}}
    end.
