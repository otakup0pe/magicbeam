-module(magicbeam).
-author('jonafree@gmail.com').

-export([main/1, behaviour_info/1, rehash/0, start/0, stop/0]).

start() -> application:start(magicbeam).
stop() -> application:stop(magicbeam).

rehash() ->
    ok = hotbeam:rehash(),
    ok = thunderbeam:rehash().

behaviour_info(callbacks) ->
    [
        {init, 0},
        {config, 3},
        {terminate, 1},
        {event, 2}
    ].

main(Args) ->
    case getopt:parse(options(), Args) of
        {error, _} -> 
            help(),
            erlang:halt(1);
        {ok, {Opts, _Rest}} -> 
            maybe_help(lists:member(help, Opts)),
            maybe_work(Opts)
    end.

maybe_help(false) -> ok;
maybe_help(true) ->
    help(),
    erlang:halt(0).

help() ->
    getopt:usage(options(), "").

options() -> 
    [
        {load, $l, "load", undefined, "Load application into remote node."},
        {unload, $u, "unload", undefined, "Unload application from remote node."},
        {help, $h, "help", undefined, "Receive Help."},
        {node, $n, "node", string, "Remote Node."},
        {cookie, $c, "cookie", string, "Cookie to Use."},
        {callback, $m, "module", string, "Callback module to register with."}
    ].

maybe_work(Opts) ->
    Node = case {node, proplists:get_value(node, Opts)} of
        {node, undefined} ->
            help(),
            erlang:halt(1);
        {node, N} -> list_to_atom(N)
    end,
    case {lists:member(load, Opts), lists:member(unload, Opts)} of
        {true, false} -> load(Node, Opts);
        {false, true} -> unload(Node, Opts);
        {_, _} ->
            help(),
            erlang:halt(1)
    end.

init(Node, Opts) ->
    application:load(magicbeam),
    ok = case net_kernel:start([magicbeam_ctl]) of
        {ok, _PID} -> ok;
        _ -> halt(1)
    end,
    case proplists:get_value(cookie, Opts) of
        undefined -> ok;
        Cookie ->
            true = erlang:set_cookie(Node, list_to_atom(Cookie)),
            true = net_kernel:hidden_connect(Node),
            ok
    end.

load(Node, Opts) ->
    ok = init(Node, Opts),
    CB = case proplists:get_value(callback, Opts) of
        undefined -> undefined;
        Mod when is_list(Mod) -> list_to_atom(Mod)
    end,
    ok = magicbeam_util:inject(Node, CB),
    io:format("Injected magicbeam.~n"),
    erlang:halt(0).

unload(Node, Opts) ->
    ok = init(Node, Opts),
    ok = magicbeam_util:remove(Node),
    io:format("Removed magicbeam.~n"),
    erlang:halt(1).
