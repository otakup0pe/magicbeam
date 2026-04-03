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

command_match_custom_type_test() ->
    %% Custom arg types (e.g. session_id) should be treated like string
    ?assert(shellbeam:command_match(
        ["signals", "show", {"id", session_id}],
        ["signals", "show", "my-session-name"]) == ["my-session-name"]),
    ?assert(shellbeam:command_match(
        ["signals", "show", {"id", session_id}],
        ["signals", "show", "abc123def456"]) == ["abc123def456"]).

command_match_custom_type_with_known_test() ->
    ?assert(shellbeam:command_match(
        ["cmd", {"id", session_id}, {"count", integer}],
        ["cmd", "my-session", "42"]) == ["my-session", 42]).

command_match_custom_type_mismatch_test() ->
    ?assert(shellbeam:command_match(
        ["signals", "show", {"id", session_id}],
        ["signals", "show"]) == false),
    ?assert(shellbeam:command_match(
        ["signals", "show", {"id", session_id}],
        ["signals", "wrong", "thing"]) == false).

shell_fun_default_test() ->
    application:unset_env(magicbeam, shell_mfa),
    Fun = magicbeam_app:shell_fun(),
    ?assert(is_function(Fun, 2)).

shell_fun_mfa_test() ->
    application:set_env(magicbeam, shell_mfa,
                        {erlang, display, [hello]}),
    Fun = magicbeam_app:shell_fun(),
    ?assert(is_function(Fun, 2)),
    application:unset_env(magicbeam, shell_mfa).

