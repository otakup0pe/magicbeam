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

upgrade_tilde_s_bare_test() ->
    ?assertEqual("~ts", shellbeam:upgrade_tilde_s("~s")).

upgrade_tilde_s_with_text_test() ->
    ?assertEqual("prefix ~ts suffix",
                 shellbeam:upgrade_tilde_s("prefix ~s suffix")).

upgrade_tilde_s_preserves_width_test() ->
    ?assertEqual("~10ts", shellbeam:upgrade_tilde_s("~10s")),
    ?assertEqual("~-10ts", shellbeam:upgrade_tilde_s("~-10s")),
    ?assertEqual("~.5ts", shellbeam:upgrade_tilde_s("~.5s")),
    ?assertEqual("~10.5ts", shellbeam:upgrade_tilde_s("~10.5s")),
    ?assertEqual("~*ts", shellbeam:upgrade_tilde_s("~*s")).

upgrade_tilde_s_leaves_literal_tilde_alone_test() ->
    %% ~~ is a literal tilde; the s that follows is NOT a format spec
    ?assertEqual("~~s", shellbeam:upgrade_tilde_s("~~s")).

upgrade_tilde_s_handles_triple_tilde_test() ->
    %% ~~~s is literal ~ followed by ~s -- the real spec must upgrade
    ?assertEqual("~~~ts", shellbeam:upgrade_tilde_s("~~~s")).

upgrade_tilde_s_multiple_in_one_string_test() ->
    ?assertEqual("~ts and ~ts",
                 shellbeam:upgrade_tilde_s("~s and ~s")),
    ?assertEqual("  Title: ~ts~n  Body: ~10ts",
                 shellbeam:upgrade_tilde_s(
                   "  Title: ~s~n  Body: ~10s")).

upgrade_tilde_s_leaves_other_specs_alone_test() ->
    ?assertEqual("~p", shellbeam:upgrade_tilde_s("~p")),
    ?assertEqual("~w", shellbeam:upgrade_tilde_s("~w")),
    ?assertEqual("~B", shellbeam:upgrade_tilde_s("~B")),
    ?assertEqual("~n", shellbeam:upgrade_tilde_s("~n")).

upgrade_tilde_s_idempotent_on_ts_test() ->
    %% Upgrading an already-upgraded string must not double-add the t
    ?assertEqual("~ts", shellbeam:upgrade_tilde_s("~ts")),
    ?assertEqual("~10ts", shellbeam:upgrade_tilde_s("~10ts")).

upgrade_tilde_s_empty_test() ->
    ?assertEqual("", shellbeam:upgrade_tilde_s("")).

upgrade_tilde_s_no_specifiers_test() ->
    ?assertEqual("plain text",
                 shellbeam:upgrade_tilde_s("plain text")).

upgrade_tilde_s_trailing_tilde_test() ->
    %% Malformed but should not crash
    ?assertEqual("foo ~", shellbeam:upgrade_tilde_s("foo ~")).

format_ts_emits_utf8_for_binary_test() ->
    %% "émdash" — and the em-dash codepoint U+2014
    Bin = unicode:characters_to_binary("dé—mo", utf8),
    Formatted = unicode:characters_to_binary(
                  io_lib:format(shellbeam:upgrade_tilde_s("~s"), [Bin])),
    ?assertEqual(Bin, Formatted).

format_ts_emits_utf8_for_codepoint_list_test() ->
    %% Codepoint list (what unicode:characters_to_list produces)
    Chars = [$d, 16#e9, $-, 16#2014, $m, $o],
    Bin = unicode:characters_to_binary(Chars, utf8),
    Formatted = unicode:characters_to_binary(
                  io_lib:format(shellbeam:upgrade_tilde_s("~s"), [Chars])),
    ?assertEqual(Bin, Formatted).

shell_fun_default_test() ->
    application:unset_env(magicbeam, shell_mfa),
    Fun = magicbeam_app:shell_fun(),
    ?assert(is_function(Fun, 2)).

shell_fun_mfa_dispatches_test() ->
    %% Verify the returned fun actually spawns and applies the
    %% configured MFA -- not just that it's shaped like a fun.
    Self = self(),
    application:set_env(magicbeam, shell_mfa,
                        {erlang, send, [Self, shell_fun_dispatched]}),
    try
        Fun = magicbeam_app:shell_fun(),
        ?assert(is_function(Fun, 2)),
        Fun(unused1, unused2),
        receive
            shell_fun_dispatched -> ok
        after 1000 ->
            ?assert(false)
        end
    after
        application:unset_env(magicbeam, shell_mfa)
    end.

shell_fun_mfa_rejects_non_mfa_test() ->
    %% A non-MFA value in shell_mfa must fall through to the default
    %% branch, not crash on function_clause.
    application:set_env(magicbeam, shell_mfa, not_an_mfa),
    try
        Fun = magicbeam_app:shell_fun(),
        ?assert(is_function(Fun, 2))
    after
        application:unset_env(magicbeam, shell_mfa)
    end.

%% strip_ansi/1: remove ANSI CSI sequences so visible-length math
%% used by format_table is accurate regardless of colour wrapping.

strip_ansi_plain_test() ->
    ?assertEqual("hello", shellbeam:strip_ansi("hello")).

strip_ansi_empty_test() ->
    ?assertEqual("", shellbeam:strip_ansi("")).

strip_ansi_colour_code_test() ->
    ?assertEqual("hello", shellbeam:strip_ansi("\e[31mhello\e[0m")).

strip_ansi_bold_test() ->
    ?assertEqual("bold", shellbeam:strip_ansi("\e[1mbold\e[0m")).

strip_ansi_multiple_test() ->
    ?assertEqual("ab",
                 shellbeam:strip_ansi("\e[31ma\e[0m\e[32mb\e[0m")).

strip_ansi_preserves_content_around_test() ->
    ?assertEqual("  hi  ",
                 shellbeam:strip_ansi("  \e[33mhi\e[0m  ")).

%% Fixture that ensures magicbeam_srv is running so ?SHELLBEAM_ANSI
%% (which does a gen_server:call) resolves without crashing. The server
%% reads `shellbeam_ansi` from appenv, so setting the env controls
%% whether colour() wraps or passes through.

with_srv_and_ansi(Ansi, Body) ->
    application:load(magicbeam),
    application:set_env(magicbeam, shellbeam_ansi, Ansi),
    {ok, Pid} = magicbeam_srv:start_link(),
    try
        Body()
    after
        unlink(Pid),
        exit(Pid, shutdown),
        application:unset_env(magicbeam, shellbeam_ansi)
    end.

colour_disabled_passthrough_test() ->
    with_srv_and_ansi(false, fun() ->
        ?assertEqual("hello", shellbeam:colour(red, "hello")),
        ?assertEqual("hello", shellbeam:colour(bold, "hello"))
    end).

colour_red_test() ->
    with_srv_and_ansi(true, fun() ->
        ?assertEqual("\e[31mhello\e[0m", shellbeam:colour(red, "hello"))
    end).

colour_basic_fg_palette_test() ->
    with_srv_and_ansi(true, fun() ->
        ?assertEqual("\e[31mr\e[0m", shellbeam:colour(red, "r")),
        ?assertEqual("\e[32mg\e[0m", shellbeam:colour(green, "g")),
        ?assertEqual("\e[33my\e[0m", shellbeam:colour(yellow, "y")),
        ?assertEqual("\e[34mb\e[0m", shellbeam:colour(blue, "b")),
        ?assertEqual("\e[35mm\e[0m", shellbeam:colour(magenta, "m")),
        ?assertEqual("\e[36mc\e[0m", shellbeam:colour(cyan, "c")),
        ?assertEqual("\e[37mw\e[0m", shellbeam:colour(white, "w"))
    end).

colour_styles_test() ->
    with_srv_and_ansi(true, fun() ->
        ?assertEqual("\e[1mbold\e[0m", shellbeam:colour(bold, "bold")),
        ?assertEqual("\e[2mdim\e[0m", shellbeam:colour(dim, "dim")),
        ?assertEqual("\e[4mu\e[0m", shellbeam:colour(underline, "u"))
    end).

format_table_single_row_test() ->
    with_srv_and_ansi(false, fun() ->
        Out = shellbeam:format_table(
                [{"name", 6}, {"age", 3}],
                [["alice", "30"]]),
        %% Three lines: header, separator, row
        Lines = string:split(Out, "\n", all),
        ?assertEqual(3, length(Lines)),
        [Header, Sep, Row] = Lines,
        %% ANSI disabled -> header is plain
        ?assertEqual("name    age", Header),
        %% Separator pads with dashes
        ?assertEqual("------  ---", Sep),
        %% Row pads columns with spaces
        ?assertEqual("alice   30 ", Row)
    end).

format_table_truncates_overflow_test() ->
    with_srv_and_ansi(false, fun() ->
        Out = shellbeam:format_table(
                [{"k", 3}],
                [["abcdefghij"]]),
        Lines = string:split(Out, "\n", all),
        [_Hdr, _Sep, Row] = Lines,
        %% Trimmed to width 3
        ?assertEqual("abc", Row)
    end).

format_table_ansi_header_alignment_test() ->
    with_srv_and_ansi(true, fun() ->
        %% Header is wrapped in bold ANSI; the separator / row widths
        %% must still line up to visible column widths (7 and 4).
        Out = shellbeam:format_table(
                [{"col1", 7}, {"col2", 4}],
                [["x", "y"]]),
        [Hdr, Sep, Row] = string:split(Out, "\n", all),
        %% Visible header content is "col1" padded to 7 + "  " + "col2"
        %% padded to 4. Strip ANSI to measure.
        ?assertEqual("col1     col2",
                     shellbeam:strip_ansi(Hdr)),
        ?assertEqual("-------  ----", Sep),
        ?assertEqual("x        y   ", Row)
    end).

%% process_command/3: return-shape normalization. Covers list vs binary
%% format strings, arg presence, error shapes, and caught exceptions.

process_command_ok_list_test() ->
    Fun = fun() -> {ok, "done"} end,
    ?assertEqual({processed, "done", []},
                 shellbeam:process_command("help", Fun, [])).

process_command_ok_binary_test() ->
    Fun = fun() -> {ok, <<"done">>} end,
    ?assertEqual({processed, "done", []},
                 shellbeam:process_command("help", Fun, [])).

process_command_ok_list_with_args_test() ->
    Fun = fun() -> {ok, "count: ~B", [42]} end,
    ?assertEqual({processed, "count: ~B", [42]},
                 shellbeam:process_command("help", Fun, [])).

process_command_ok_binary_with_args_test() ->
    Fun = fun() -> {ok, <<"count: ~B">>, [42]} end,
    ?assertEqual({processed, "count: ~B", [42]},
                 shellbeam:process_command("help", Fun, [])).

process_command_syntax_shape_test() ->
    Fun = fun() -> syntax end,
    ?assertEqual({error, "Syntax Error. ~s", ["help-text"]},
                 shellbeam:process_command("help-text", Fun, [])).

process_command_error_list_test() ->
    Fun = fun() -> {error, "boom"} end,
    ?assertEqual({error, "boom", []},
                 shellbeam:process_command("help", Fun, [])).

process_command_error_binary_with_args_test() ->
    Fun = fun() -> {error, <<"boom ~B">>, [1]} end,
    ?assertEqual({error, "boom ~B", [1]},
                 shellbeam:process_command("help", Fun, [])).

process_command_subshell_passthrough_test() ->
    ?assertEqual({subshell, [mod_a, mod_b], "sub"},
                 shellbeam:process_command(
                   "help",
                   {subshell, [mod_a, mod_b], "sub"},
                   [])).

process_command_passes_args_to_callback_test() ->
    Fun = fun(X, Y) -> {ok, "~p+~p", [X, Y]} end,
    ?assertEqual({processed, "~p+~p", [1, 2]},
                 shellbeam:process_command("help", Fun, [1, 2])).

process_command_catches_error_class_test() ->
    Fun = fun() -> error(crashed) end,
    ?assertEqual({error, "Exception while processing command", []},
                 shellbeam:process_command("help", Fun, [])).

