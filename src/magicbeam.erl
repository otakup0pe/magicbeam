-module(magicbeam).
-author('jonafree@gmail.com').

-export([main/1, behaviour_info/1, rehash/0, start/0, stop/0, info/0, rpc_shell/1]).

start() -> application:start(magicbeam).
stop() -> application:stop(magicbeam).

info() ->
    [
     {hotbeam, hotbeam:info()},
     {thunderbeam, thunderbeam:info()}
    ].

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
     {short, $s, "short", undefined, "Use short names."},
     {cookie, $c, "cookie", string, "Cookie to Use."},
     {callback, $m, "module", string, "Callback module to register with."},
     {shell, $r, "shell", undefined, "Start shell on remote node."}
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
            case lists:member(shell, Opts) of
                true -> shell(Node, Opts);
                false ->
                    help(),
                    erlang:halt(1)
            end
    end.

init(Node, Opts) ->
    application:load(magicbeam),
    NKSO = [magicbeam_ctl] ++ case lists:member(short, Opts) of
                                  false -> [longnames];
                                  true -> [shortnames]
                              end,
    ok = case net_kernel:start(NKSO) of
             {ok, _PID} -> ok
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

shell(Node, Opts) ->
    ok = init(Node, Opts),
    block_until_done(user_drv:start(['tty_sl -c -e', {magicbeam, rpc_shell, [Node]}])),
    ok.

rpc_shell(Node) ->
    rpc:call(Node, shellbeam, spawn_shell, []).

block_until_done(PID) ->
    case is_process_alive(PID) of
        true ->
	    timer:sleep(500),
            block_until_done(PID);
        false ->
	    init:stop()
    end.
