-module(magicbeam).
-author('jonafree@gmail.com').

-export([main/1, behaviour_info/1, rehash/0, start/0, stop/0, info/0, rpc_shell/1, remote_shell/1]).

start() ->
    ok = magicbeam_util:start_deps(),
    ok = application:start(magicbeam).

stop() -> ok = application:stop(magicbeam).

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
     {short, undefined, "short", undefined, "Use short names."},
     {setcookie, $c, "cookie", string, "Cookie to Use."},
     {callback, undefined, "module", string, "Callback module to register with."},
     {shell, $s, "shell", undefined, "Start shell on remote node."}
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
        {_, false} -> maybe_shell(Node, Opts)
    end.

maybe_shell(Node, Opts) ->
    case lists:member(shell, Opts) of
        true -> shell(Node, Opts);
        false ->
            erlang:halt(0)
    end.

init(Node, Opts) ->
    application:load(magicbeam),
    NKSO = [magicbeam_ctl] ++ case lists:member(short, Opts) of
                                  false -> [longnames];
                                  true -> [shortnames]
                              end,
    ok = case net_kernel:start(NKSO) of
             {ok, _PID} -> ok;
             {error, {already_started, _PID}} -> ok
         end,
    case proplists:get_value(setcookie, Opts) of
        undefined -> connect(Node);
        Cookie ->
            true = erlang:set_cookie(Node, list_to_atom(Cookie)),
            connect(Node)
    end.

connect(Node) ->
    EF = fun() -> magicbeam_util:error_out("Unable to establish connection with " ++ atom_to_list(Node)) end,
    case net_kernel:hidden_connect(Node) of
        true ->
            case net_adm:ping(Node) of
                pong -> ok;
                pang -> EF()
            end;
        false ->
            EF()
    end.

load(Node, Opts) ->
    ok = init(Node, Opts),
    CB = case proplists:get_value(callback, Opts) of
             undefined -> undefined;
             Mod when is_list(Mod) -> list_to_atom(Mod)
         end,
    ok = magicbeam_util:inject(Node, CB),
    io:format("Injected magicbeam.~n"),
    maybe_shell(Node, Opts).

unload(Node, Opts) ->
    ok = init(Node, Opts),
    ok = magicbeam_util:remove(Node),
    io:format("Removed magicbeam.~n"),
    erlang:halt(1).

shell(Node, Opts) ->
    ok = init(Node, Opts),
    case magicbeam_util:loaded(Node) of
        true -> remote_shell(Node);
        false -> magicbeam_util:error_out("Application not loaded on " ++ atom_to_list(Node))
    end.

remote_shell(Node) ->
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
