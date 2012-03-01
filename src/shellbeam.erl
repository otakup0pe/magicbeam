-module(shellbeam).

-include("magicbeam.hrl").

-export([test/0]).
-export([start_shell/0, start_shell/2, shell_helper/2]).

test() ->
    application:start(sasl),
    application:start(magicbeam),
    start_shell().

start_shell() ->
    start_shell([magicbeam_shell], ?SHELLBEAM_PROMPT).
start_shell(Modules, Prompt) when is_list(Modules), is_list(Prompt) ->
    title(?SHELLBEAM_TITLE),
    case proc_lib:spawn_opt(?MODULE, shell_helper, [self(), Modules], [link]) of
        PID when is_pid(PID) ->
            handle_shell(PID, 0, Prompt)
    end.

handle_shell(PID, I, Prompt) ->
    case io:get_line(colour(green, Prompt) ++ " " ++ colour(red, integer_to_list(I)) ++ " > ") of
        eof -> ok;
        {error, _} = E -> error_out("Unable to get_line -> ~p", [E]), handle_shell(PID, I, Prompt);
        D when is_list(D) ->
            case string:tokens(string:strip(D, right, $\n), " ") of
                [] ->
                    handle_shell(PID, I, Prompt);
                T when is_list(T) ->
                    PID ! {process, T},
                    receive
                        {processed, F, A} ->
                            io:format(F ++ "~n", A),
                            handle_shell(PID, I + 1, Prompt);
                        {error, F, A} ->
                            error_out(F, A),
                            handle_shell(PID, I + 1, Prompt);
                        exit ->
                            ok;
                        {subshell, M, P} ->
                            start_shell(M, P),
                            handle_shell(PID, I + 1, Prompt)
                    end
            end
    end.

shell_helper(PID, Modules) ->
    loop(PID, scan_modules(Modules)).

scan_modules(Modules) -> scan_modules(Modules, []).
scan_modules([], Commands) ->
    Commands;
scan_modules([Module | Tail], Commands) ->
    case Module:commands() of
        C when is_list(C) ->
            scan_modules(Tail, C ++ Commands)
    end.

loop(PID, Commands) ->
    receive
        {process, Tokens} ->
            case process_loop(PID, Commands, Tokens) of
                syntax -> PID ! {error, "Syntax Error.~n" ++ p_syntax(Commands), []};
                ok -> ok
            end,
            loop(PID, Commands)
    end.

process_loop(PID, _, ["exit"]) ->
    PID ! exit,
    ok;
process_loop(PID, C, ["help"]) ->
    PID ! {processed, "Help.~n" ++ p_syntax(C), []},
    ok;
process_loop(_PID, [], _Tokens) -> syntax;
process_loop(PID, [{[H | _], Help, {subshell, Mods, _} = E} | _], [H|Tokens]) when length(Tokens) > 0 ->
    C = scan_modules(Mods),
    case process_loop(PID, C, Tokens) of
        syntax -> PID ! {error, "Syntax Error.~n" ++ p_syntax(C), []}, ok;
        T -> T
    end;
process_loop(PID, [{Match, Help, C} = E| CTail], Tokens) ->
    case command_match(Match, Tokens) of
        false ->
            process_loop(PID, CTail, Tokens);
        syntax ->
            PID ! {error, "Syntax Error.~n" ++ p_syntax([E]), []}, ok;
        A when is_list(A) ->
            process_command(PID, Help, C, A)
    end;
process_loop(PID, [_ | CTail], Tokens) ->
    process_loop(PID, CTail, Tokens).

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
process_command(PID, Help, CFun, Ar) when is_function(CFun) ->
    case catch apply(CFun, Ar) of
        {ok, F} when is_list(F) ->
            PID ! {processed, F, []}, ok;
        {ok, F, A} when is_list(F), is_list(A) ->
            PID ! {processed, F, A}, ok;
        syntax -> PID ! { error, "Syntax Error. ~s", [Help]}, ok;
        {error, F} when is_list(F) -> PID ! {error, F, []}, ok;
        {error, F, A} when is_list(F), is_list(A) -> PID ! {error, F, A}, ok
    end;
process_command(PID, _Help, {subshell, M, P}, _Ar) ->
    PID ! {subshell, M, P},
    ok.

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
