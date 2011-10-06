-module(magicbeam_util).
-include("magicbeam.hrl").

-export([cfgget/2, cfgset/2, inject/1, remove/1]).

cfgget(Key, Default) ->
    case application:get_env(magicbeam, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.
cfgset(Key, Value) ->
    ok = application:set_env(magicbeam, Key, Value).

mods() ->
    {ok, Mods} = application:get_key(magicbeam, modules),
    Mods.

inject(Node) -> inject(Node, mods()).
inject(Node, []) -> ok = rpc:call(Node, magicbeam_app, rpc_start, [app_spec()]);
inject(Node, [Mod | Mods]) when is_atom(Mod) ->
    io:format("Injecting ~p~n", [Mod]),
    {Mod, Bin, FName} = code:get_object_code(Mod),
    {module, Mod} = rpc(Node, code, load_binary, [Mod, FName, Bin]),
    inject(Node, Mods).

app_spec() ->
    {ok, Keys} = application:get_all_key(magicbeam),
    {application, magicbeam, Keys ++ [{env, application:get_all_env(magicbeam)}]}.

remove(Node) -> 
    ok = rpc(Node, magicbeam_app, rpc_stop, []),
    remove(Node, mods()).
remove(_Node, []) -> ok;
remove(Node, [Mod | Mods]) when is_atom(Mod) ->
    rpc(Node, code, purge, [Mod]),
    rpc(Node, code, delete, [Mod]),
    remove(Node, Mods).

rpc(Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, _E} -> error;
        {error, _E} -> error;
        Value -> Value
    end.