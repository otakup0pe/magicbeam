%% @author Jonathan Freedman <jonafree@gmail.com>
%% @copyright (c) 2012 ExactTarget, 2013-2026 Jonathan Freedman
%% @doc REPL for the rest of us
%%
%% The shellbeam behaviour allows a developer to create a user-friendly shell. There is a single
%% function expected by this behaviour. It is expected to return a list of command definitions
%% The command is Mod:commands()
%%
%% A command definition is a three part tuple. The first part is the string tokens to watch for.
%% This includes the various arguments and their term types for parsing. Token components can be
%% either bare strings or a two part tuple for arguments. For arguments, the first element in the
%% tuple is simply the name used by the online help. The second is the type of the argument for
%% matching and parsing. It may be one of either string, integer, bool, atom or auto. Atoms must
%% already exist. Auto will attempt to determine exactly what the data type is. It first attempts
%% a conversion to an integer then an (existing) atom. Failing this it will attempt to convert to
%% a list or tuple and finally it falls back to a string.
%%
%% The second part is simply the command description used for the online help.
%%
%% The third part is the actuall command handler. It may be one of two items. The most common
%% item is a fun which accepts as many arguments as were defined in tokens. This fun will return
%% a tuple indicating either an ok or error state, and a format string. For example {ok, "OK", []}
%% or {error, "Not OK", []}. The third element may also be a subshell tuple which will have one of
%% two effects. If arguments follow the defined tokens then command matching will proceed into the
%% subshell. If no arguments than a new subshell will be opened with a different prompt. The
%% subshell tuple has three elements. The first is the atom subshell, the second is a list of
%% shellbeam callback modules and the third is the prompt to use.
%% @end

-module(shellbeam).
-author('jonafree@gmail.com').

-include("magicbeam.hrl").

-export([behaviour_info/1]).
-export([start_shell/2, spawn_shell/2, spawn_shell/0]).
-export([colour/2, format_table/2]).
-export([expand_fun/1]).
-export([collect_line/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

behaviour_info(callbacks) ->

    [
     {commands,0}
    ].

%% @spec spawn_shell() -> ok
%% @doc Will spawn a shell from an interactive Erlang console.
spawn_shell() ->
    case magicbeam_util:appenv(shell_mfa, undefined) of
        {M, F, A} when is_atom(M), is_atom(F), is_list(A) ->
            spawn(fun() -> apply(M, F, A), erlang:halt() end);
        _ ->
            spawn_shell(?SHELLBEAM_MODULES, ?SHELLBEAM_PROMPT)
    end.

%% @private
spawn_shell(Modules, Prompt) ->
    spawn(fun() -> start_shell(Modules, Prompt), erlang:halt() end).

%% @private
start_shell(Modules, Prompt) when is_list(Modules), is_list(Prompt) ->
    io:format("Magicbeam Shell v~s~n", [erlang:system_info(version)]),
    Commands = scan_modules(Modules),
    ExpandFun = fun(ReversedLine) -> expand_fun(ReversedLine, Commands, Modules) end,
    io:setopts([{encoding, unicode}, {expand_fun, ExpandFun}]),
    handle_shell(0, Commands, Prompt),
    terminated.

%% @doc Core Loop. Prints prompt, converts string to tokens and attempts to process command.
%%
%% Will take one of five options depending on results.
%%   * Generate magicbeam event and output results
%%   * Generate error message either from attempted command
%%   * Generate syntax error in case of invalid command
%%   * Exit
%%   * Spawn a subshell
handle_shell(I, Commands, Prompt) ->
    ColorPrompt = colour(green, Prompt) ++ " " ++ colour(red, integer_to_list(I)) ++ " > ",
    case get_line_with_history(ColorPrompt) of
        eof -> ok;
        {error, _} = E -> error_out("Unable to read input -> ~p", [E]), handle_shell(I, Commands, Prompt);
        D when is_list(D) ->
            case string:tokens(string:strip(D, right, $\n), " ") of
                [] ->
                    handle_shell(I, Commands, Prompt);
                T when is_list(T) ->
                    case process_tokens(Commands, T) of
                        {processed, F, A} ->
                            magicbeam_srv:event({shellbeam, processed, T}),
                            normal_out(F, A),
                            handle_shell(I + 1, Commands, Prompt);
                        syntax ->
                            error_out("Syntax Error.~n" ++ p_syntax(Commands), []),
                            handle_shell(I + 1, Commands, Prompt);
                        {error, F, A} ->
                            error_out(F, A),
                            handle_shell(I + 1, Commands, Prompt);
                        exit ->
                            ok;
                        {subshell, M, P} ->
                            magicbeam_srv:event({shellbeam, subshell, M}),
                            ok = handle_shell(0, scan_modules(M), P),
                            handle_shell(I + 1, Commands, Prompt)
                    end
            end
    end.

scan_modules(Modules) -> scan_modules(Modules, []).

%% @doc Generates command listing based on callback modules provided
scan_modules([], Commands) ->
    Commands;
scan_modules([Module | Tail], Commands) ->
    case {command_prefix(Module), Module:commands()} of
        {undefined, C} when is_list(C) ->
            scan_modules(Tail, C ++ Commands);
        {P, C} when is_list(P), is_list(C) ->
            Cm = lists:map(fun({T, H, F}) -> {P ++ T, H, F} end, C),
            scan_modules(Tail, Cm ++ Commands)
    end.

command_prefix(Module) ->
    case lists:member({prefix, 0}, Module:module_info(exports)) of
        false ->
            [];
        true ->
            case Module:prefix() of
                L when is_list(L) ->
                    L
            end
    end.

%% @doc Processes string tokens. May recurse into subshells.
process_tokens(_, ["exit"]) ->
    exit;
process_tokens(C, ["help"]) ->
    {processed, "Help.~n" ++ p_syntax(C), []};
process_tokens([], _Tokens) ->
    syntax;
process_tokens([{H, _, {subshell, Mods, _}} | CTail], Tokens) when length(Tokens) > length(H) ->
    case lists:split(length(H), Tokens) of
        {H, T} ->
            C = scan_modules(Mods),
            case process_tokens(C, T) of
                syntax -> {error, "Syntax Error.~n" ++ p_syntax(C), []};
                R -> R
            end;
        {L, _T} when is_list(L) -> process_tokens(CTail, Tokens)
    end;
process_tokens([{Match, Help, C} = E| CTail], Tokens) ->
    case command_match(Match, Tokens) of
        false ->
            process_tokens(CTail, Tokens);
        syntax ->
            {error, "Syntax Error.~n" ++ p_syntax([E]), []};
        A when is_list(A) ->
            process_command(Help, C, A)
    end;
process_tokens([_ | CTail], Tokens) ->
    process_tokens(CTail, Tokens).

command_match(C, T) -> command_match(C, T, []).
%% @doc Processes commands. Performs type conversion where required by command tokens.
command_match([], [], A) -> A;
command_match([H | MT], [H | TT], A) when is_list(H) ->
    command_match(MT, TT, A);
command_match([{_, atom} | MT], [H | TT], Ar) ->
    try list_to_existing_atom(H) of
        A when is_atom(A) -> command_match(MT, TT, Ar ++ [A])
    catch
        error:badarg -> syntax
    end;
command_match([{_, bool} | MT], [H | TT], Ar) ->
    try list_to_existing_atom(string:to_lower(H)) of
        A when A == true; A == false -> command_match(MT, TT, Ar ++ [A]);
        _ -> syntax
    catch
        error:badarg -> syntax
    end;
command_match([{_, integer} | MT], [H | TT], Ar) ->
    try list_to_integer(H) of
        I when is_integer(I) -> command_match(MT, TT, Ar ++ [I])
    catch
        error:badarg -> syntax
    end;
command_match([{_, any} | MT], [H | TT], Ar) ->
    command_match(MT, TT, Ar ++ [H]);
command_match([{_, _, optional} | MT], [_H | _] = TT, Ar) ->
    command_match(MT, TT, Ar);
command_match([{_, auto} | MT], [H | TT], Ar) ->
    command_match(MT, TT, Ar ++ [distill_item(H)]);
command_match([{_, string}|MT], [[$"|_]|_] = TT, Ar) ->
    {STail, S} = distill_string(TT),
    command_match(MT, STail, Ar ++ [string:join(S, " ")]);
command_match([{_, string}], TT, Ar) ->
    command_match([], [], Ar ++ [string:join(TT, " ")]);
command_match([{_, string}|MT], [H|T], Ar) ->
    command_match(MT, T, Ar ++ [H]);
command_match([{_, _Type}|MT], [H|T], Ar) ->
    command_match(MT, T, Ar ++ [H]);
command_match(_, _, _) -> false.

%% @doc Extracts a (double) quoted string
distill_string(TT) ->
    distill_string(TT, []).
distill_string([], TT) ->
    {[], lists:reverse(TT)};
distill_string([[$"|H]|T], TT) ->
    distill_string(T, [H|TT]);
distill_string([H|T], TT) ->
    case string:substr(H, length(H), 1) of
        [$"] ->
            {T, lists:reverse([string:substr(H, 1, length(H) - 1)|TT])};
        _ ->
            distill_string(T, [H|TT])
    end.


%% @doc Attempts to automagically convert to a proper term
distill_item(H) ->
    try list_to_integer(H) of
        I -> I
    catch
        error:badarg ->
            try list_to_existing_atom(H) of
                A -> A
            catch
                error:badarg ->
                    case {hd(H), hd(lists:reverse(H))} of
                        {$[, $]} when length(H) > 2 -> distill_list(H);
                        {${, $}} when length(H) > 2 -> list_to_tuple(distill_list(H));
                        _ -> H
                    end
            end
    end.

distill_list(H) -> distill_list(string:tokens(lists:sublist(H, 2, length(H) - 2), ","), []).
distill_list([], O) -> O;
distill_list([H | T], O) ->
    distill_list(T, O ++ [distill_item(H)]).

process_command(Help, CFun, Ar) when is_function(CFun) ->
    try apply(CFun, Ar) of
        {ok, F} when is_list(F) ->
            {processed, F, []};
        {ok, F, A} when is_list(F), is_list(A) ->
            {processed, F, A};
        syntax -> {error, "Syntax Error. ~s", [Help]};
        {error, F} when is_list(F) -> {error, F, []};
        {error, F, A} when is_list(F), is_list(A) -> {error, F, A}
    catch
        _:E ->
            ?error("process_command exception ~p:~p - ~p", [CFun, Ar, E]),
            {error, "Exception while processing command", []}
    end;
process_command(_Help, {subshell, M, P}, _Ar) ->
    {subshell, M, P}.

error_out(F, A) ->
    normal_out(colour(red, "Problems: ") ++ F, A).

normal_out(F, A) ->
    try io:format(F ++ "~n", A) of
        ok -> ok
    catch
        _:E ->
            ?error("normal_out exception ~p ~p - ~p", [F, A, E]),
            error_out("Exception while handling output", [])
    end.

-define(COLOURIZE(C, S), "\e[3" ++ integer_to_list(C) ++ "m" ++ S ++ "\e[0m").
%% @doc who doesn't like colors
colour(Colour, Text) when is_list(Text) ->
    case ?SHELLBEAM_ANSI of
        false ->
            Text;
        true -> p_colour1(Colour, Text)
    end.
p_colour1(red, Text) -> ?COLOURIZE(1, Text);
p_colour1(green, Text) -> ?COLOURIZE(2, Text);
p_colour1(yellow, Text) -> ?COLOURIZE(3, Text);
p_colour1(blue, Text) -> ?COLOURIZE(4, Text);
p_colour1(magenta, Text) -> ?COLOURIZE(5, Text);
p_colour1(cyan, Text) -> ?COLOURIZE(6, Text);
p_colour1(white, Text) -> ?COLOURIZE(7, Text);
p_colour1(bold, Text) -> "\e[1m" ++ Text ++ "\e[0m";
p_colour1(dim, Text) -> "\e[2m" ++ Text ++ "\e[0m";
p_colour1(underline, Text) -> "\e[4m" ++ Text ++ "\e[0m".

%% @doc Format a list of rows as an aligned table.
%% Headers is a list of {Name, Width} tuples.
%% Rows is a list of lists of strings (same length as Headers).
%% Returns a formatted string ready for io:format.
format_table(Headers, Rows) ->
    HdrNames = [N || {N, _} <- Headers],
    Widths = [W || {_, W} <- Headers],
    Sep = format_row(lists:duplicate(length(Headers), ""), Widths, $-),
    HdrLine = colour(bold, format_row(HdrNames, Widths, $ )),
    RowLines = [format_row(R, Widths, $ ) || R <- Rows],
    string:join([HdrLine, Sep | RowLines], "\n").

format_row(Cols, Widths, Pad) ->
    format_row(Cols, Widths, Pad, []).

format_row([], [], _Pad, Acc) ->
    lists:flatten(lists:reverse(Acc));
format_row([Col | CT], [Width | WT], Pad, Acc) ->
    %% Strip ANSI codes to measure visible length for padding/slicing.
    Visible = strip_ansi(Col),
    VisLen = string:length(Visible),
    %% Trim only if the visible text exceeds the column width.
    {Trimmed, TrimmedVisible} = case VisLen > Width of
        true ->
            T = string:slice(Visible, 0, Width),
            {T, T};
        false ->
            {Col, Visible}
    end,
    %% Pad based on visible length so ANSI codes don't throw off alignment.
    PadChar = case Pad of $- -> $-; _ -> $  end,
    PadAmount = max(0, Width - string:length(TrimmedVisible)),
    Padded = Trimmed ++ lists:duplicate(PadAmount, PadChar),
    Spacer = case WT of
        [] -> "";
        _  -> "  "
    end,
    format_row(CT, WT, Pad, [Spacer, Padded | Acc]);
format_row(_, _, _, Acc) ->
    lists:flatten(lists:reverse(Acc)).

%% @doc Strip ANSI escape sequences from a string for visible-length measurement.
strip_ansi(Str) ->
    strip_ansi(Str, [], false).
strip_ansi([], Acc, _InEsc) ->
    lists:reverse(Acc);
strip_ansi([$\e | Rest], Acc, _InEsc) ->
    strip_ansi(Rest, Acc, true);
strip_ansi([C | Rest], Acc, true) ->
    %% Inside an escape sequence; letters (a-z, A-Z) terminate it.
    case (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) of
        true  -> strip_ansi(Rest, Acc, false);
        false -> strip_ansi(Rest, Acc, true)
    end;
strip_ansi([C | Rest], Acc, false) ->
    strip_ansi(Rest, [C | Acc], false).

%% @doc Read a line via the IO protocol's get_until request, which
%% triggers group.erl's save_line_buffer (unlike io:get_line which
%% skips history). io:get_until/4 was removed in OTP 27 but the
%% underlying IO protocol request is still handled by group.erl.
get_line_with_history(Prompt) ->
    GL = group_leader(),
    Ref = make_ref(),
    GL ! {io_request, self(), Ref,
          {get_until, unicode, Prompt, shellbeam, collect_line, []}},
    receive
        {io_reply, Ref, Result} ->
            Result
    end.

%% @doc Callback for io:get_until/5. Collects characters until a
%% newline is found, then returns the complete line.
collect_line(Cont, eof) ->
    case Cont of
        [] -> {done, eof, []};
        _ -> {done, Cont ++ "\n", []}
    end;
collect_line(Cont, Chars) ->
    case lists:splitwith(fun(C) -> C =/= $\n end, Chars) of
        {Line, []} ->
            {more, Cont ++ Line};
        {Line, [$\n | Rest]} ->
            {done, Cont ++ Line ++ "\n", Rest}
    end.

%% @doc Single-argument version for external callers / testing.
%% Requires commands to be scanned separately.
expand_fun(ReversedLine) ->
    expand_fun(ReversedLine, []).

%% @doc Tab completion callback for io:setopts expand_fun.
%% ReversedLine is the current line reversed (edlin convention).
%% Commands is the scanned command list from scan_modules/1.
expand_fun(ReversedLine, Commands) ->
    expand_fun(ReversedLine, Commands, []).

%% @doc Tab completion with callback module support.
%% Modules that export arg_completions/1 can provide completions for argument types.
expand_fun(ReversedLine, Commands, Modules) ->
    Line = lists:reverse(ReversedLine),
    Tokens = string:tokens(Line, " "),
    %% Detect whether user has a trailing space (completing next token vs current)
    TrailingSpace = case Line of
        [] -> false;
        _ -> lists:last(Line) =:= $\s
    end,
    case {Tokens, TrailingSpace} of
        {[], _} ->
            %% Empty line -- show all command names
            Names = lists:usort(extract_command_names(Commands)),
            {no, [], format_alternatives(Names)};
        {Toks, true} ->
            %% Trailing space means current tokens are complete, completing next token
            expand_next_token(Toks, Commands, Modules);
        {Toks, false} ->
            %% No trailing space -- completing the last partial token
            Partial = lists:last(Toks),
            Prefix = lists:sublist(Toks, length(Toks) - 1),
            expand_partial_token(Prefix, Partial, Commands, Modules)
    end.

%% Complete a partial (last) token given the preceding complete tokens.
expand_partial_token([], Partial, Commands, _Modules) ->
    %% Completing first word -- match command names
    Names = lists:usort(extract_command_names(Commands)),
    complete_from_list(Partial, Names);
expand_partial_token(Prefix, Partial, Commands, Modules) ->
    %% A single depth can offer both literal sub-commands and an
    %% argument slot across different command definitions. Merge
    %% candidates from both sources before filtering by Partial --
    %% short-circuiting on the first arg-slot match would hide a
    %% literal sibling token from completion entirely.
    ArgCompletions = case expects_argument(Prefix, Commands) of
        {true, Type} -> fetch_arg_completions(Type, Modules);
        false -> []
    end,
    Literals = literal_subcommands_at_depth(Prefix, Commands),
    Candidates = lists:usort(ArgCompletions ++ Literals),
    complete_from_list(Partial, Candidates).

%% Complete the next token when all prior tokens are complete (trailing space).
%% A single depth can have BOTH an argument slot (one command definition)
%% and literal sub-command tokens (another command definition). Merge
%% completions from both sources so tab shows everything valid at this
%% depth, not just whichever entry expects_argument/2 happened to match
%% first.
expand_next_token(Toks, Commands, Modules) ->
    ArgCompletions = case expects_argument(Toks, Commands) of
        {true, Type} -> fetch_arg_completions(Type, Modules);
        false -> []
    end,
    Literals = literal_subcommands_at_depth(Toks, Commands),
    All = lists:usort(ArgCompletions ++ Literals),
    case All of
        [] -> {no, [], []};
        _ -> {no, [], format_alternatives(All)}
    end.

%% Collect the literal sub-command tokens that appear at the position
%% immediately after Toks across all command definitions.
literal_subcommands_at_depth(Toks, Commands) ->
    AllPrefixes = extract_all_token_sequences(Commands),
    Depth = length(Toks) + 1,
    [lists:nth(Depth, Seq)
     || Seq <- AllPrefixes,
        length(Seq) >= Depth,
        lists:sublist(Seq, length(Toks)) =:= Toks,
        is_list(lists:nth(Depth, Seq))].

%% Check whether the given complete tokens match a command definition
%% up to a point where the next token is an argument.
%% Returns {true, Type} where Type is the argument type atom, or false.
expects_argument(Toks, Commands) ->
    expects_argument(Toks, Commands, none).
expects_argument(_Toks, [], _Best) ->
    false;
expects_argument(Toks, [{TokenDef, _Help, _Fun} | Rest], Best) ->
    case match_prefix_for_arg(Toks, TokenDef) of
        {true, Type} -> {true, Type};
        false -> expects_argument(Toks, Rest, Best)
    end.

%% Walk a command's token definition checking if Toks consumes all the
%% literal tokens and the next position is an argument slot.
match_prefix_for_arg([], [{_Name, Type} | _]) ->
    ArgType = classify_arg_type(Type),
    {true, ArgType};
match_prefix_for_arg([T | TRest], [T | DRest]) when is_list(T) ->
    match_prefix_for_arg(TRest, DRest);
match_prefix_for_arg(_, _) ->
    false.

%% Map argument types to a classification for completion purposes.
%% "permalink" and generic "string" args on lookup-like commands -> permalink.
classify_arg_type(string) -> string;
classify_arg_type(atom) -> atom;
classify_arg_type(integer) -> integer;
classify_arg_type(bool) -> bool;
classify_arg_type(auto) -> auto;
classify_arg_type(any) -> any;
classify_arg_type(Other) -> Other.

%% Extract the first literal token from each command definition.
extract_command_names(Commands) ->
    lists:filtermap(fun
        ({[First | _], _Help, _Fun}) when is_list(First) -> {true, First};
        (_) -> false
    end, Commands).

%% Extract full token sequences (literal strings only) for multi-word matching.
extract_all_token_sequences(Commands) ->
    lists:map(fun({TokenDef, _Help, _Fun}) -> TokenDef end, Commands).

%% Query callback modules for argument completions.
%% Each module may export arg_completions/1 returning a list of strings
%% for the given argument type atom.
fetch_arg_completions(Type, Modules) ->
    lists:usort(lists:flatmap(fun(Mod) ->
        case erlang:function_exported(Mod, arg_completions, 1) of
            true ->
                try Mod:arg_completions(Type)
                catch _:_ -> []
                end;
            false -> []
        end
    end, Modules)).

%% Given a partial string and a list of candidates, compute the completion.
complete_from_list(_Partial, []) ->
    {no, [], []};
complete_from_list(Partial, Candidates) ->
    Matches = [C || C <- Candidates, lists:prefix(Partial, C)],
    case Matches of
        [] ->
            {no, [], []};
        [Single] ->
            Expansion = lists:nthtail(length(Partial), Single),
            {yes, Expansion ++ " ", []};
        Multiple ->
            CommonPrefix = longest_common_prefix(Multiple),
            Expansion = lists:nthtail(length(Partial), CommonPrefix),
            case Expansion of
                [] ->
                    {no, [], format_alternatives(Multiple)};
                _ ->
                    {yes, Expansion, format_alternatives(Multiple)}
            end
    end.

%% Compute the longest common prefix of a list of strings.
longest_common_prefix([]) -> "";
longest_common_prefix([S]) -> S;
longest_common_prefix([S | Rest]) ->
    lists:foldl(fun common_prefix/2, S, Rest).

common_prefix([], _B) -> [];
common_prefix(_A, []) -> [];
common_prefix([C | AT], [C | BT]) -> [C | common_prefix(AT, BT)];
common_prefix(_, _) -> [].

%% Format alternatives for display by the shell.
format_alternatives([]) -> [];
format_alternatives(Names) ->
    lists:map(fun(N) -> N end, lists:sort(Names)).

p_syntax(C) -> p_syntax(C, "help - this command~nexit - leave current shell~n").
p_syntax([], O) -> O;
p_syntax([{C, H, _} | T], O) ->
    p_syntax(T, O ++ p_render_command(C) ++ "- " ++ H ++ "~n");
p_syntax([H | T], O) ->
    ?warn("unknown command definition ~p", [H]),
    p_syntax(T, O).


p_render_command(C) ->
    p_render_command(C, "").
p_render_command([], O) -> O;
p_render_command([W | T], O) when is_list(W) ->
    p_render_command(T, O ++ W ++ " ");
p_render_command([{W, Ty} | T], O) when is_list(W), is_atom(Ty) ->
    p_render_command(T, O ++ "[" ++ W ++ " (" ++ atom_to_list(Ty) ++ ")] ").
