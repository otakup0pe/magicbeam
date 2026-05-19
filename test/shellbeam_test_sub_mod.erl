-module(shellbeam_test_sub_mod).
-export([commands/0]).

commands() ->
    [{["show", {"id", string}], "Show detail", noop},
     {["list"], "List all", noop}].
