-module(shellbeam_test).

-include_lib("eunit/include/eunit.hrl").

distill_string_test() ->
    ?assert(shellbeam:distill_item("what") == "what").

distill_atom_test() ->
    ?assert(shellbeam:distill_item("distill_item") == distill_item).

distill_integer_test() ->
    ?assert(shellbeam:distill_item("42") == 42).

distill_tuple_test() ->
    ?assert(shellbeam:distill_item("{1,2,3}") == {1,2,3}).

distill_list_test() ->
    ?assert(shellbeam:distill_item("[1,2,3]") == [1,2,3]).

command_match_base_test() ->
    ?assert(shellbeam:command_match(["test"], ["test"]) == []).

command_match_atom_test() ->
    ?assert(shellbeam:command_match(["test", {"", atom}], ["test", "shellbeam"]) == [shellbeam]),
    ?assert(shellbeam:command_match(["test", {"", atom}], ["test", "what"]) == syntax).

command_match_bool_test() ->
    ?assert(shellbeam:command_match(["test", {"", bool}], ["test", "false"]) == [false]),
    ?assert(shellbeam:command_match(["test", {"", bool}], ["test", "true"]) == [true]),
    ?assert(shellbeam:command_match(["test", {"", bool}], ["test", "what"]) == syntax).

command_match_integer_test() ->
    ?assert(shellbeam:command_match(["test", {"", integer}], ["test", "42"]) == [42]),
    ?assert(shellbeam:command_match(["test", {"", integer}], ["test", "test"]) == syntax).

command_match_mixed_test() ->
    ?assert(shellbeam:command_match(["test", {"", atom}, {"", integer}], ["test", "shellbeam", "42"]) == [shellbeam, 42]).

