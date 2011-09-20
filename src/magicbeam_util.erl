-module(magicbeam_util).
-include("magicbeam.hrl").

-export([app_env/2, inject/1]).

app_env(Key, Default) ->
    case application:get_env(magicbeam, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

inject_app(Node) -> inject_app(Node, {ok, Mods} = application:get_key(magicbeam, modules)).
inject_app(Node, {ok, Mods}) when is_list(Mods) -> inject_app(Node, Mods);
inject_app(Node, []) -> ok;
inject_app(Node, [Mod | Mods]) when is_atom(Mod) ->
    {Mod, Bin, FName} = code:get_object_code(Mod),
    {module, Mod} = rpc:call(Node, code, load_binary, [Mod, FName, Bin]),
    inject_app(Node, Mods).

rpc(Node, M, F, A) ->
    case rpc:call(Host, M, F, A) of
        {ok, Value} -> Value;
        {badrpc, _E} -> error;
        {error, _E} -> error
    end.