%% @author Jonathan Freedman <jonafree@gmail.com>
%% @copyright (c) 2012 ExactTarget
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

-ifdef(TEST).
-compile(export_all).
-endif.

behaviour_info(callbacks) ->

    [
     {commands,0}
    ].

%% @spec spawn_shell() -> ok
%% @doc Will spawn a shell from an interactive Erlang console
spawn_shell() ->
    spawn_shell(?SHELLBEAM_MODULES, ?SHELLBEAM_PROMPT).

%% @private
spawn_shell(Modules, Prompt) ->
    spawn(fun() -> start_shell(Modules, Prompt) end).

%% @private
start_shell(Modules, Prompt) when is_list(Modules), is_list(Prompt) ->
    io:format("Magicbeam Shell v~s~n", [erlang:system_info(version)]),
    handle_shell(0, scan_modules(Modules), Prompt).

%% @doc Core Loop. Prints prompt, converts string to tokens and attempts to process command.
%%
%% Will take one of five options depending on results.
%%   * Generate magicbeam event and output results
%%   * Generate error message either from attempted command
%%   * Generate syntax error in case of invalid command
%%   * Exit
%%   * Spawn a subshell
handle_shell(I, Commands, Prompt) ->
    case io:get_line(colour(green, Prompt) ++ " " ++ colour(red, integer_to_list(I)) ++ " > ") of
        eof -> ok;
        {error, _} = E -> error_out("Unable to get_line -> ~p", [E]), handle_shell(I, Commands, Prompt);
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
process_tokens([], Tokens) -> 
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
    case catch list_to_existing_atom(H) of
        A when is_atom(A) -> command_match(MT, TT, Ar ++ [A]);
        {'EXIT',{badarg,_}} -> syntax
    end;
command_match([{_, bool} | MT], [H | TT], Ar) ->
    case catch list_to_existing_atom(string:to_lower(H)) of
        A when A == true; A == false -> command_match(MT, TT, Ar ++ [A]);
        A when is_atom(A) -> syntax;
	{'EXIT', {badarg, _}} -> syntax
    end;
command_match([{_, integer} | MT], [H | TT], Ar) ->
    case catch list_to_integer(H) of
        I when is_integer(I) -> command_match(MT, TT, Ar ++ [I]);
        {'EXIT', {badarg, _}} -> syntax
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
command_match([{_, string}|MT], [H|T], Ar) ->
    command_match(MT, T, Ar ++ [H]);
command_match([{_, string}], TT, Ar) ->
    command_match([], [], Ar ++ [string:join(TT, " ")]);
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
    case catch list_to_integer(H) of
        I when is_integer(I) -> I;
        {'EXIT',{badarg, _}} ->
            case catch list_to_existing_atom(H) of
                A when is_atom(A) -> A;
                {'EXIT',{badarg,_}} ->
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
    case catch apply(CFun, Ar) of
        {ok, F} when is_list(F) ->
            {processed, F, []};
        {ok, F, A} when is_list(F), is_list(A) ->
            {processed, F, A};
        syntax -> { error, "Syntax Error. ~s", [Help]};
        {error, F} when is_list(F) -> {error, F, []};
        {error, F, A} when is_list(F), is_list(A) -> {error, F, A};
        {'EXIT', E} ->
            ?error("process_command exception ~p:~p - ~p", [CFun, Ar, E]),
            {error, "Exception while processing command", []}
    end;
process_command(_Help, {subshell, M, P}, _Ar) ->
    {subshell, M, P}.

error_out(F, A) ->
    normal_out(colour(red, "Problems: ") ++ F, A).

normal_out(F, A) ->
    case catch io:format(F ++ "~n", A) of
        ok ->
            ok;
        {'EXIT', E} ->
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
p_colour1(blue, Text) -> ?COLOURIZE(4, Text).

                                        %maybe support this again sometime?
                                        %title(Text) when is_list(Text) ->
                                        %    case ?SHELLBEAM_ANSI of
                                        %        false -> ok;
                                        %        true -> io:format("\e]0;" ++ Text ++ "\007")
                                        %    end.

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
