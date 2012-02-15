-module(magicbeam_shell).
-behaviour(shellbeam).

-export([commands/0]).
-export([hotload/1, thunderdome/1, setenv/3]).

commands() ->
    [
     {["hotload", {"module", atom}], "Recompile and reload a beam", fun hotload/1},
     {["thunderdome", {"enable", bool}], "Enable/Disable aggressive thunderbeam activity", fun thunderdome/1},
     {["setenv", {"app", atom}, {"key", atom}, {"value", any}], "Set an environment variable", fun setenv/3}
    ].

hotload(M) when is_atom(M) ->
    ok = hotbeam:mod(M),
    {ok, "Hot reload of ~p requested", [M]};
hotload(_) -> syntax.

thunderdome(true) ->
    thunderbeam:enabled(true),
    {ok, "Thunderbeam enabled"};
thunderdome(false) ->
    thunderbeam:enabled(false),
    {ok, "Thunderbeam disabled"};
thunderdome(_) -> syntax.

setenv(A, K, V) when is_atom(A), is_atom(K) ->
    application:set_env(A, K, p_distill_val(V)),
    {ok, "Set ~p:~p to ~p", [A, K, V]}.

p_distill_val(V) ->
    case catch list_to_integer(V) of
        I when is_integer(I) ->
            I;
        {'EXIT',{badarg,[{erlang,list_to_integer,[V]} | _]}} ->
            case catch list_to_existing_atom(string:to_lower(V)) of
                A when is_atom(A) ->
                    A;
                {'EXIT',{badarg,[{erlang,list_to_existing_atom,[V]} | _]}} ->
                    V
            end
    end.
