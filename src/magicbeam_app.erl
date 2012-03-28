%% @private
-module(magicbeam_app).
-author('jonafree@gmail.com').
-export([start/2, stop/1, rpc_start/1, rpc_stop/0]).

-include("magicbeam.hrl").

-record(state, {ssh}).

rpc_start(AppSpec) ->
    ok = application:load(AppSpec),
    case magicbeam_util:start_deps() of
        ok -> ok = application:start(magicbeam);
        {error, _} = E -> E
    end.

rpc_stop() ->
    application:stop(magicbeam),
    application:unload(magicbeam),
    ok.

stop(#state{ssh = PID}) when is_pid(PID) ->
    ssh:stop_daemon(PID),
    ok;
stop(_) -> ok.

start(_Type, _Args) ->
    {ok, SupPid} = magicbeam_sup:start_link(),
    {ok, SupPid, case magicbeam_util:appenv(ssh, false) of
                     false -> #state{};
                     true -> start_ssh()
                 end}.

start_ssh() ->
    Port = magicbeam_util:appenv(ssh_port, 4422),
    case ssh_file_path() of
        undefined -> undefined;
        SshPath when is_list(SshPath) ->
            ssh:daemon(Port, [
                              {system_dir, SshPath},
                              {user_dir, SshPath},
                              {nodelay, true},
                              {shell, fun(_, _) -> shellbeam:spawn_shell() end}
                             ])
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
