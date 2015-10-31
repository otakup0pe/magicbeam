%% @author Jonathan Freedman <jonafree@gmail.com>
%% @copyright (c) 2012 ExactTarget
%% @private

%% @doc A variety of utilities used throughout the magicbeam application
%% @end

-module(magicbeam_util).
-include("magicbeam.hrl").
-author('jonafree@gmail.com').

-export([appenv/2, inject/4, remove/1, error_out/1, loaded/1, start_deps/0, ssh_file_path/0, random/3, random/2]).

%% @spec random(Min::integer(), Max::integer()) -> Value::integer()
%% @doc Returns a random number between Min and Max
random(Min, Max) ->
    random(Min, Max, 0).

%% @spec random(Min::integer(), Max::integer(), Base::integer()) -> Value::integer()
%% @doc Returns a random number of Base + something between Min and Max
random(Min, Max, Base) when is_integer(Min), is_integer(Max), is_integer(Base) ->
    case erlang:get(random_seed) of
	{_, _, _} ->
	    random1(Min, Max, Base);
	undefined ->
	    {A, B, C} = erlang:now(),
	    random:seed(A, B, C),
	    random1(Min, Max, Base)
    end.
random1(Min, Max, Base) ->
    case random:uniform(Max) of
	I when I >= Min ->
	    I + Base;
	I when is_integer(I) ->
	    random1(Min, Max, Base)
    end.
%% @spec appenv(Key::atom(), Default::term()) -> Value::term()
%% @doc Returns either the application environment variable or a (hopefully sensible) default value
appenv(Key, Default) ->
    case application:get_env(magicbeam, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

%% @spec mods() -> [Mod::atom()]
%% @doc Returns list of modules included with magicbeam. Used during app injection.
mods() ->
    {ok, Mods} = application:get_key(magicbeam, modules),
    Mods.

%% @spec inject(Node::atom(), Mod::atom(), SSH::atom(), [Mod::atom()]) -> ok | error | {error, Reason::term()}
%% @doc Attempts to inject the magicbeam application into a remote node
%%      First step involves starting dependencies. Second step involves loading
%%      beam files. Third step involves starting application based on the
%%      application tuple as opposed to the app file normally included.
inject(Node, Mod, SSH, ShellMods) ->
    case loaded(Node) of
        false -> inject(Node, Mod, SSH, ShellMods, mods());
        true -> error_out("Already loaded magicbeam on " ++ atom_to_list(Node))
    end.
%% @private
inject(Node, Mod, SSH, ShellMods, []) ->
    case rpc(Node, magicbeam_app, rpc_start, [app_spec(Mod, SSH, ShellMods)]) of
        ok -> ok;
        {error, {magicbeam, {start_dep, App, _}}} -> error_out("Unable to start dependency " ++ atom_to_list(App) ++ " on " ++ atom_to_list(Node));
        E when E == error ; is_tuple(E) -> error_out("Unable to start application on " ++ atom_to_list(Node))
    end;
inject(Node, Mod, SSH, ShellMods, [M | Tail]) when is_atom(Mod) ->
    {M, Bin, FName} = code:get_object_code(M),
    case rpc(Node, code, load_binary, [M, FName, Bin]) of
        {module, M} -> inject(Node, Mod, SSH, ShellMods, Tail);
        error -> error_out("Unable to inject on " ++ atom_to_list(Node))
    end.

%% @doc Generates application spec
app_spec(Mod, SSH, ShellMods) ->
    {ok, Keys} = application:get_all_key(magicbeam),
    Env = [{shellbeam_modules, ShellMods}] ++ if
	       is_atom(Mod) ; Mod /= undefined -> [{callback, Mod}] ; 
	       true -> []
	   end ++
	case SSH of
	    false -> [];
	    {ssh, Path, Port} ->
		[{ssh_port, Port}, {ssh_path, Path}, {ssh, true}]
	end,
    {application,
     magicbeam,
     lists:keystore(env, 1, Keys, {env, Env})
    }.

%% @spec remove(Node::atom()) -> ok | error
%% @doc Attempts to remove application from remote node
remove(Node) ->
    case loaded(Node) of
        false -> remove(Node, mods());
        true ->
            case rpc(Node, magicbeam_app, rpc_stop, []) of
                ok -> remove(Node, mods());
                error -> error_out("Unable to load magicbeam on " ++ atom_to_list(Node))
            end
    end,
    remove(Node, mods()).
%% @private
remove(_Node, []) -> ok;
remove(Node, [Mod | Mods]) when is_atom(Mod) ->
    rpc(Node, code, purge, [Mod]),
    rpc(Node, code, delete, [Mod]),
    remove(Node, Mods).

%% @doc Simple RPC wrapper
rpc(Node, M, F, A) ->
    case rpc:call(Node, M, F, A) of
        {badrpc, _E} -> error;
	{error, {magicbeam, _}} = E -> E;
        {error, _} -> error;
        Value -> Value
    end.

%% @doc Used at during the command line bits. Spits out error and returns with exit code.
error_out(M) ->
    io:format("Error: " ++ M ++ "~n"),
    erlang:halt(1).

%% @spec loaded(Node::atom()) -> true | false
%% @doc Checks remote node to see if magicbeam is loaded
loaded(Node) ->
    case rpc(Node, application, which_applications, []) of
        L when is_list(L) ->
            case lists:keysearch(magicbeam, 1, L) of
                {value, {magicbeam, _, _}} ->
                    true;
                false ->
                    false
            end;
        error -> false
    end.

%% @private
start_deps() ->
    case application:get_key(magicbeam, applications) of
        undefined -> {error, {magicbeam, not_loaded}};
        {ok, Deps} -> start_dep(Deps)
    end.

%% @private
start_dep([]) -> ok;
start_dep([App | Deps]) ->
    application:load(App),
    case application:get_key(App, applications) of
        {ok, AppDeps} -> 
            start_dep(AppDeps);
        undefined -> ok
    end,
    case application:start(App) of
        ok -> start_dep(Deps);
        {error,{already_started,App}} -> start_dep(Deps);
        {error, R} -> {error, {magicbeam, {start_dep, App, R}}}
    end.

ssh_file_path() ->
    P0 = case os:getenv("HOME") of
             L when is_list(L) -> L;
             false -> "/tmp"
         end ++ "/.magicbeam/",
    Path = ssh_appenv_path(P0),
    case file:list_dir(Path) of
        {error, enoent} -> ssh_init_path(Path);
        {ok, _F} -> ssh_verify_path(Path)
    end.

ssh_appenv_path(P0) ->
    case magicbeam_util:appenv(ssh_path, P0) of
        P0 -> P0;
        [root | Tail] -> code:root_dir() ++ Tail;
        Path when is_list(Path) -> Path
    end.


ssh_init_path(Path) ->
    F = fun(Type) ->
                case os:cmd("ssh-keygen -t " ++ Type ++ " -f " ++ Path ++ "ssh_host_" ++ Type ++ "_key -N ''") of
                    A when is_list(A) -> ok
                end
        end,
    case file:make_dir(Path) of
        ok ->
            F("rsa"), F("dsa"),
            ?warn("please edit authorized_keys", [])
    end,
    Path.

ssh_verify_path(Path) ->
    F = fun(File) ->
                case file:read_file_info(Path ++ File) of
                    {ok, _} ->
                        true;
                    {error, _} ->
                        false
                end
        end,
    case {F("authorized_keys"), F("ssh_host_dsa_key"), F("ssh_host_rsa_key")} of
        {true, true, true} ->
            Path;
        {false, true, true} ->
            ?warn("missing file ~s", [Path ++ "authorized_keys"]),
            Path;
        {A, B, C} ->
            ?error("missing magicbeam ssh files in path ~p authorized_keys:~p ssh_host_dsa_key:~p ssh_host_rsa_key:~p", [Path, A, B, C]),
            undefined
    end.
