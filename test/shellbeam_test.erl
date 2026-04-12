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
    %% Unknown arg types should be treated like single-token strings
    ?assert(shellbeam:command_match(
        ["item", "show", {"id", custom_id}],
        ["item", "show", "abc-123"]) == ["abc-123"]),
    ?assert(shellbeam:command_match(
        ["item", "show", {"id", custom_id}],
        ["item", "show", "xyz-456"]) == ["xyz-456"]).

command_match_custom_type_with_known_test() ->
    ?assert(shellbeam:command_match(
        ["cmd", {"id", custom_id}, {"count", integer}],
        ["cmd", "abc-123", "42"]) == ["abc-123", 42]).

command_match_custom_type_mismatch_test() ->
    ?assert(shellbeam:command_match(
        ["item", "show", {"id", custom_id}],
        ["item", "show"]) == false),
    ?assert(shellbeam:command_match(
        ["item", "show", {"id", custom_id}],
        ["item", "wrong", "thing"]) == false).

command_match_string_greedy_single_token_test() ->
    ?assert(shellbeam:command_match(
        ["search", {"q", string}],
        ["search", "apple"]) == ["apple"]).

command_match_string_greedy_multi_token_test() ->
    ?assert(shellbeam:command_match(
        ["search", {"q", string}],
        ["search", "apple", "banana", "cherry"])
            == ["apple banana cherry"]).

command_match_string_greedy_empty_test() ->
    ?assert(shellbeam:command_match(
        ["search", {"q", string}],
        ["search"]) == [""]).

command_match_string_non_trailing_test() ->
    ?assert(shellbeam:command_match(
        [{"first", string}, {"count", integer}],
        ["hello", "42"]) == ["hello", 42]).

command_match_string_quoted_test() ->
    ?assert(shellbeam:command_match(
        [{"q", string}, {"count", integer}],
        ["\"hello", "world\"", "42"]) == ["hello world", 42]).

%% Tab completion: literal sub-commands and arg slots at the same
%% depth must be merged, not short-circuited.

sibling_fixture_commands() ->
    [{["list", "items", "by", {"id", string}], "", noop},
     {["list", "items", {"count", integer}], "", noop},
     {["list", "items"], "", noop}].

literal_subcommands_merges_across_entries_test() ->
    ?assertEqual(
        ["by"],
        shellbeam:literal_subcommands_at_depth(
            ["list", "items"], sibling_fixture_commands())).

literal_subcommands_ignores_arg_slots_test() ->
    ?assertEqual(
        [],
        shellbeam:literal_subcommands_at_depth(
            ["x"], [{["x", {"y", integer}], "", noop}])).

literal_subcommands_picks_from_multiple_entries_test() ->
    Cmds = [{["a", "b"], "", noop},
            {["a", "c"], "", noop},
            {["a", "d", "e"], "", noop}],
    ?assertEqual(
        ["b", "c", "d"],
        lists:sort(shellbeam:literal_subcommands_at_depth(["a"], Cmds))).

expand_next_token_merges_literal_and_arg_test() ->
    {no, [], Alts} = shellbeam:expand_next_token(
        ["list", "items"], sibling_fixture_commands(), []),
    ?assert(lists:member("by", Alts)).

expand_partial_token_completes_literal_test() ->
    %% The regression: integer arg slot at this depth must not
    %% hide the "by" literal from completion of "b<TAB>".
    ?assertMatch(
        {yes, "y ", _},
        shellbeam:expand_partial_token(
            ["list", "items"], "b",
            sibling_fixture_commands(), [])).

expand_next_token_arg_only_still_works_test() ->
    ?assertEqual(
        {no, [], []},
        shellbeam:expand_next_token(
            ["x"], [{["x", {"n", integer}], "", noop}], [])).

expand_next_token_literals_only_still_works_test() ->
    Cmds = [{["x", "a"], "", noop}, {["x", "b"], "", noop}],
    {no, [], Alts} = shellbeam:expand_next_token(["x"], Cmds, []),
    ?assertEqual(["a", "b"], lists:sort(Alts)).

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

