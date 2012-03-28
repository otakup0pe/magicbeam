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
    SshPath = ?SSH_PATH,
    ssh:daemon(?SSH_PORT, [
                           {system_dir, SshPath},
                           {user_dir, SshPath},
                           {nodelay, true},
                           {shell, fun(_, _) -> shellbeam:spawn_shell() end}
                          ]).

