-module(magicbeam_util).
-include("magicbeam.hrl").
-author('jonafree@gmail.com').

-export([appenv/2, inject/2, remove/1, error_out/1, loaded/1]).

appenv(Key, Default) ->
    case application:get_env(magicbeam, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

mods() ->
    {ok, Mods} = application:get_key(magicbeam, modules),
    Mods.

inject(Node, Mod) ->
    case loaded(Node) of
        false -> inject(Node, Mod, mods());
        true -> error_out("Already loaded magicbeam on " ++ atom_to_list(Node))
    end.
inject(Node, Mod, []) ->
    case rpc(Node, magicbeam_app, rpc_start, [app_spec(Mod)]) of
        ok -> ok;
        error -> error_out("Unable to start application on " ++ atom_to_list(Node))
    end;
inject(Node, Mod, [M | Tail]) when is_atom(Mod) ->
    {M, Bin, FName} = code:get_object_code(M),
    case rpc(Node, code, load_binary, [M, FName, Bin]) of
        {module, M} -> inject(Node, Mod, Tail);
        error -> error_out("Unable to inject on " ++ atom_to_list(Node))
    end.

app_spec(Mod) ->
    {ok, Keys} = application:get_all_key(magicbeam),
    AS = {application,
          magicbeam,
          if is_atom(Mod) ; Mod /= undefined -> lists:keystore(env, 1, Keys, {env, application:get_all_env(magicbeam) ++ [{callback, Mod}]}) ; true -> Keys end},
    AS.

remove(Node) ->
    case loaded(Node) of
        false -> error_out("Magicbeam is not loaded on " ++ atom_to_list(Node));
        true ->
            case rpc(Node, magicbeam_app, rpc_stop, []) of
                ok -> ok;
                error -> error_out("Unable to load magicbeam on " ++ atom_to_list(Node))
            end
    end,
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

error_out(M) ->
    io:format("Error: " ++ M ++ "~n"),
    erlang:halt(1).

loaded(Node) ->
    case rpc(Node, application, which_applications, []) of
        L when is_list(L) ->
            case lists:keysearch(magicbeam, 1, L) of
                {value, {magicbeam, _, _}} ->
                    true;
                false ->
                    false
            end
    end.
