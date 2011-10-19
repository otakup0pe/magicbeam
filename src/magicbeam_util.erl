-module(magicbeam_util).
-include("magicbeam.hrl").

-export([appenv/2, inject/2, remove/1]).

appenv(Key, Default) ->
    case application:get_env(magicbeam, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

mods() ->
    {ok, Mods} = application:get_key(magicbeam, modules),
    Mods.

inject(Node, Callback) -> inject(Node, Callback, mods()).
inject(Node, Callback, []) -> ok = rpc:call(Node, magicbeam_app, rpc_start, [app_spec(Callback)]);
inject(Node, Callback, [Mod | Mods]) when is_atom(Mod) ->
    {Mod, Bin, FName} = code:get_object_code(Mod),
    {module, Mod} = rpc(Node, code, load_binary, [Mod, FName, Bin]),
    inject(Node, Callback, Mods).

app_spec(Callback) ->
    {ok, Keys} = application:get_all_key(magicbeam),
    {application, magicbeam, Keys ++ 
        [{env, application:get_all_env(magicbeam)}] ++ if is_atom(Callback) -> [{callback, Callback}] ; true -> [] end}.

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