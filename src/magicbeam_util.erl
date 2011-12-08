-module(magicbeam_util).
-include("magicbeam.hrl").
-author('jonafree@gmail.com').

-export([appenv/2, inject/2, remove/1]).

appenv(Key, Default) ->
    V = case application:get_env(magicbeam, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

mods() ->
    {ok, Mods} = application:get_key(magicbeam, modules),
    Mods.

inject(Node, Mod) -> inject(Node, Mod, mods()).
inject(Node, Mod, []) -> ok = rpc:call(Node, magicbeam_app, rpc_start, [app_spec(Mod)]);
inject(Node, Mod, [M | Tail]) when is_atom(Mod) ->
    {M, Bin, FName} = code:get_object_code(M),
    {module, M} = rpc(Node, code, load_binary, [M, FName, Bin]),
    inject(Node, Mod, Tail).

app_spec(Mod) ->
    {ok, Keys} = application:get_all_key(magicbeam),
    AS = {application, 
        magicbeam, 
        if is_atom(Mod) ; Mod /= undefined -> lists:keystore(env, 1, Keys, {env, application:get_all_env(magicbeam) ++ [{callback, Mod}]}) ; true -> Keys end},
    AS.

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