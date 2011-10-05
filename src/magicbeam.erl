-module(magicbeam).

-export([main/1]).

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
        undefined ->
            help(),
            erlang:halt(1);
        N -> N
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
    case proplists:get_value(cookie, Opts) of
        undefined -> ok;
        Cookie ->
            true = erlang:set_cookie(Node, Cookie),
            ok
    end.

load(Node, Opts) ->
    ok = init(Node, Opts),
    ok = magicbeam_util:inject(Node),
    io:format("Injected magicbeam."),
    erlang:halt(0).

unload(Node, Opts) ->
    ok = init(Node, Opts),
    ok = magicbeam_util:remove(Node),
    io:format("Removed magicbeam."),
    erlang:halt(1).
