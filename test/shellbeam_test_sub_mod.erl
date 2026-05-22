-module(shellbeam_test_sub_mod).
-export([commands/0, arg_completions/1]).

commands() ->
    [{["show", {"id", custom_id}], "Show detail", noop},
     {["list"], "List all", noop}].

arg_completions(custom_id) ->
    ["abc-123", "abc-456", "def-789"];
arg_completions(_) ->
    [].
