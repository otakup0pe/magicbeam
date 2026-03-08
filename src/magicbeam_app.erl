%% @private
-module(magicbeam_app).
-author('jonafree@gmail.com').
-export([start/2, stop/1, rpc_start/1, rpc_stop/0, restart_ssh/0]).

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
    case SshPath of
        undefined ->
            ?error("ssh path is undefined, cannot start ssh daemon", []),
            #state{};
        _ ->
            case ssh:daemon(?SSH_PORT, [
                                        {key_cb, {shellbeam_keys, [{key_dir, SshPath}]}},
                                        {user_dir, SshPath},
                                        {preferred_algorithms, [{public_key, ['ssh-ed25519']}]},
                                        {nodelay, true},
                                        {shell, fun(_, _) -> spawn(fun() -> shellbeam:start_shell(?SHELLBEAM_MODULES, ?SHELLBEAM_PROMPT) end) end}
                                       ]) of
                {ok, Pid} ->
                    application:set_env(magicbeam, ssh_daemon, Pid),
                    ?info("ssh daemon started on port ~p", [?SSH_PORT]),
                    #state{ssh = Pid};
                {error, Reason} ->
                    ?error("ssh daemon failed to start: ~p", [Reason]),
                    #state{}
            end
    end.

restart_ssh() ->
    case application:get_env(magicbeam, ssh_daemon) of
        {ok, OldPid} when is_pid(OldPid) ->
            ssh:stop_daemon(OldPid),
            application:unset_env(magicbeam, ssh_daemon),
            ?info("stopped ssh daemon ~p", [OldPid]);
        _ ->
            ok
    end,
    case start_ssh() of
        #state{ssh = Pid} when is_pid(Pid) ->
            {ok, Pid};
        #state{} ->
            {error, "ssh daemon failed to restart"}
    end.

