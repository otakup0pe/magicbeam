-module(shellbeam).

-include("magicbeam.hrl").

-export([test/0]).
-export([behaviour_info/1]).
-export([start_shell/0, start_shell/2, start/0, spawn_shell/0]).

test() ->
    application:start(sasl),
    application:start(magicbeam).

behaviour_info(callbacks) ->

    [
     {commands,0}
    ].

start() ->
    application:start(sasl), application:start(crypto), application:start(ssh), application:start(magicbeam),
    timer:sleep(2000),
    user_drv:start(['tty_sl -c -e', {shellbeam, spawn_shell, []}]).

spawn_shell() ->
    spawn(fun() -> start_shell() end).

start_shell() ->
    start_shell([magicbeam_shell], ?SHELLBEAM_PROMPT).
start_shell(Modules, Prompt) when is_list(Modules), is_list(Prompt) ->
    title(?SHELLBEAM_TITLE),
    io:format("Magicbeam Shell v~s~n", [erlang:system_info(version)]),
    handle_shell(0, scan_modules(Modules), Prompt).

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
                            io:format(F ++ "~n", A),
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
                            ok = handle_shell(0, M, P),
                            handle_shell(I + 1, Commands, Prompt)
                    end
            end
    end.

scan_modules(Modules) -> scan_modules(Modules, []).
scan_modules([], Commands) ->
    Commands;
scan_modules([Module | Tail], Commands) ->
    case Module:commands() of
        C when is_list(C) ->
            scan_modules(Tail, C ++ Commands)
    end.

process_tokens(_, ["exit"]) ->
    exit;
process_tokens(C, ["help"]) ->
    {processed, "Help.~n" ++ p_syntax(C), []};
process_tokens([], _Tokens) -> syntax;
process_tokens([{[H | _], _, {subshell, Mods, _}} | _], [H|Tokens]) when length(Tokens) > 0 ->
    C = scan_modules(Mods),
    case process_tokens(C, Tokens) of
        syntax -> {error, "Syntax Error.~n" ++ p_syntax(C), []};
        T -> T
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
command_match([], [], A) -> A;
command_match([H | MT], [H | TT], A) when is_list(H) ->
    command_match(MT, TT, A);
command_match([{_, atom} | MT], [H | TT], Ar) ->
    case catch list_to_existing_atom(H) of
        A when is_atom(A) -> command_match(MT, TT, Ar ++ [A]);
        {'EXIT',{badarg,[{erlang,list_to_existing_atom,[H]} | _]}} -> syntax
    end;
command_match([{_, bool} | MT], [H | TT], Ar) ->
    case catch list_to_existing_atom(string:to_lower(H)) of
        A when A == true; A == false -> command_match(MT, TT, Ar ++ [A]);
        A when is_atom(A) -> syntax
    end;
command_match([{_, any} | MT], [H | TT], Ar) ->
    command_match(MT, TT, Ar ++ [H]);
command_match([{_, auto} | MT], [H | TT], Ar) ->
    case catch list_to_integer(H) of
        I when is_integer(I) -> command_match(MT, TT, Ar ++ [I]);
        {'EXIT',{badarg,[{erlang,list_to_integer,[H]} | _]}} ->
            case catch list_to_existing_atom(H) of
                A when is_atom(A) -> command_match(MT, TT, Ar ++ [A]);
                {'EXIT',{badarg,[{erlang,list_to_existing_atom,[H]} | _]}} -> command_match(MT, TT, Ar ++ [H])
            end
    end;
command_match(_, _, _) -> false.
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
	    ?error("Exception when interpreting command - ~p", [E]),
	    {error, "Exception", []}
    end;
process_command(_Help, {subshell, M, P}, _Ar) ->
    {subshell, M, P}.

error_out(F, A) ->
    io:format(colour(red, "Problems: ") ++ F, A).

-define(COLOURIZE(C, S), "\e[3" ++ integer_to_list(C) ++ "m" ++ S ++ "\e[0m").
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

title(Text) when is_list(Text) ->
    case ?SHELLBEAM_ANSI of
        false -> ok;
        true -> io:format("\e]0;" ++ Text ++ "\007")
    end.

p_syntax(C) -> p_syntax(C, "").
p_syntax([], O) -> O;
p_syntax([{C, H, _} | T], O) ->
    p_syntax(T, O ++ p_render_command(C) ++ "- " ++ H ++ "~n").

p_render_command(C) ->
    p_render_command(C, "").
p_render_command([], O) -> O;
p_render_command([W | T], O) when is_list(W) ->
    p_render_command(T, O ++ W ++ " ");
p_render_command([{W, Ty} | T], O) when is_list(W), is_atom(Ty) ->
    p_render_command(T, O ++ "[" ++ W ++ " (" ++ atom_to_list(Ty) ++ ")] ").
