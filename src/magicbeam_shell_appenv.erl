%% @private
-module(magicbeam_shell_appenv).
-behaviour(shellbeam).

-export([commands/0]).
-export([list/0, list/1, get/2, set/3]).

commands() ->
    [
     {["list"], "Lists loaded OTP applications", fun ?MODULE:list/0},
     {["list", {"application", atom}], "Lists environment for an application", fun ?MODULE:list/1},
     {["get", {"application", atom}, {"key", atom}], "Displays a key for an OTP Application", fun ?MODULE:get/2},
     {["set", {"application", atom}, {"key", atom}, {"value", auto}], "Sets an OTP Application environment key", fun ?MODULE:set/3}
    ].

list() ->
    F = fun({A, _, _}) ->
                atom_to_list(A) ++ "~n" end,
    {ok, lists:flatten("Available Applications.~n" ++ lists:map(F, application:loaded_applications()))}.

list(A) when is_atom(A) ->
    F = fun({included_application, _}) ->
                "";
           ({K, V}) -> io_lib:format("~p : ~p~n", [K, V])
        end,
    {ok, lists:flatten("Application Environment for " ++ atom_to_list(A) ++ "~n" ++ lists:map(F, application:get_all_env(A)))}.

get(A, K) ->
    V = case application:get_env(A, K) of
            undefined -> undefined;
            {ok, T} -> T
        end,
    {ok, "~p:~p is ~p", [A, K, V]}.

set(A, K, V) ->
    ok = application:set_env(A, K, V),
    {ok, "Set ~p:~p to ~p", [A, K, V]}.
