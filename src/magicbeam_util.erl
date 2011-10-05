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

inject(Node) -> inject(Node, ?MAGICBEAM_MODULES).
inject(Node, []) -> ok = rpc:call(Node, magicbeam_app, rpc_start, [app_spec()]);
inject(Node, [Mod | Mods]) when is_atom(Mod) ->
    {Mod, Bin, FName} = code:get_object_code(Mod),
    {module, Mod} = rpc:call(Node, code, load_binary, [Mod, FName, Bin]),
    inject(Node, Mods).

app_spec() ->
    {ok, Keys = application:get_all_key(magicbeam)},
    {application, magicbeam, Keys ++ [{env, application:get_all_env(magicbeam)}]}.

remove(Node) -> 
    ok = rpc:call(Node, magicbeam_app, rpc_stop, []),
    remove(?MAGICBEAM_MODULES).
remove(Node, [Mod | Mods]) when is_atom(Mod) ->
    rpc:call(Node, code, delete, [Mod]),
    rpc:call(Node, code, purge, [Mod]),
    remove(Node, Mods).

rpc(Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
        {ok, Value} -> Value;
        {badrpc, _E} -> error;
        {error, _E} -> error
    end.