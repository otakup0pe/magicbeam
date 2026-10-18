-module(shellbeam_ssh_SUITE).

-behaviour(shellbeam).
-behaviour(ssh_client_key_api).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([ssh_shell_editing_and_exit/1]).
-export([commands/0, start_shell/0]).
-export([user_key/2, is_host_key/5, add_host_key/4]).

-define(TIMEOUT, 5000).

all() ->
    [ssh_shell_editing_and_exit].

init_per_suite(Config) ->
    ok = load_magicbeam(),
    SshDir = filename:join(?config(priv_dir, Config), "ssh"),
    ok = filelib:ensure_dir(filename:join(SshDir, ".keep")),
    ClientKey = write_authorized_key(SshDir),
    ok = application:set_env(magicbeam, ssh, true),
    ok = application:set_env(magicbeam, ssh_port, 0),
    ok = application:set_env(magicbeam, ssh_path, SshDir),
    ok = application:set_env(
        magicbeam, shell_mfa, {?MODULE, start_shell, []}),
    ok = application:set_env(magicbeam, shellbeam_ansi, false),
    {ok, _Started} = application:ensure_all_started(magicbeam),
    {ok, Daemon} = application:get_env(magicbeam, ssh_daemon),
    {port, Port} = ssh:daemon_info(Daemon, port),
    [{ssh_port, Port}, {client_key, ClientKey} | Config].

end_per_suite(_Config) ->
    ok = application:stop(magicbeam),
    lists:foreach(
        fun(Key) -> application:unset_env(magicbeam, Key) end,
        [ssh, ssh_port, ssh_path, shell_mfa, shellbeam_ansi]),
    ok.

ssh_shell_editing_and_exit(Config) ->
    Port = ?config(ssh_port, Config),
    ClientKey = ?config(client_key, Config),
    Options = [
        {user, "magicbeam-ct"},
        {key_cb, {?MODULE, [{private_key, ClientKey}]}},
        {silently_accept_hosts, true},
        {user_interaction, false},
        {preferred_algorithms, [{public_key, ['ssh-ed25519']}]}
    ],
    {ok, Connection} =
        ssh:connect({127, 0, 0, 1}, Port, Options, ?TIMEOUT),
    try
        {ok, Channel} =
            ssh_connection:session_channel(Connection, ?TIMEOUT),
        success = ssh_connection:ptty_alloc(
            Connection,
            Channel,
            [{term, "xterm"}, {width, 80}, {height, 24}, {pty_opts, []}],
            ?TIMEOUT),
        ok = ssh_connection:shell(Connection, Channel),
        _ = await_contains(
            Connection, Channel, <<"ssh 0 > ">>, <<>>, deadline()),

        ok = send_keys(
            Connection, Channel,
            [<<"echo ac">>, <<"\e[D">>, <<"b">>, <<"\r">>]),
        _ = await_contains(
            Connection, Channel, <<"ECHO:abc">>, <<>>, deadline()),

        ok = send_keys(
            Connection, Channel, [<<"\e[A">>, <<"\r">>]),
        _ = await_contains(
            Connection, Channel, <<"ECHO:abc">>, <<>>, deadline()),

        ok = send_keys(
            Connection, Channel,
            [<<"discard">>, <<21>>, <<"echo cleared">>, <<"\r">>]),
        _ = await_contains(
            Connection, Channel, <<"ECHO:cleared">>, <<>>, deadline()),

        ok = send_keys(
            Connection, Channel,
            [<<"echo tail">>, <<1>>, <<11>>,
             <<"echo replaced">>, <<"\r">>]),
        _ = await_contains(
            Connection, Channel, <<"ECHO:replaced">>, <<>>, deadline()),

        ok = send_keys(
            Connection, Channel,
            [<<"ec">>, <<"\t">>, <<"complete">>, <<"\r">>]),
        _ = await_contains(
            Connection, Channel, <<"ECHO:complete">>, <<>>, deadline()),

        ok = send_keys(
            Connection, Channel, [<<"quit">>, <<"\r">>]),
        FinalOutput =
            await_closed(Connection, Channel, <<>>, deadline()),
        assert_contains(FinalOutput, <<"Goodbye from SSH">>),
        assert_not_contains(FinalOutput, <<"ssh 6 > ">>)
    after
        ok = ssh:close(Connection)
    end.

commands() ->
    [
        {["echo", {"text", string}], "Echo text", fun echo/1},
        {["quit"], "Exit the shell", fun quit/0}
    ].

start_shell() ->
    shellbeam:start_shell(
        [?MODULE], "ssh", #{banner => "Magicbeam SSH test"}).

echo(Text) ->
    {ok, "ECHO:~ts", [Text]}.

quit() ->
    {exit, "Goodbye from SSH", []}.

user_key('ssh-ed25519', Options) ->
    PrivateOptions =
        proplists:get_value(key_cb_private, Options, []),
    {ok, proplists:get_value(private_key, PrivateOptions)};
user_key(_Algorithm, _Options) ->
    {error, "unsupported algorithm"}.

is_host_key(_Key, _Host, _Port, _Algorithm, _Options) ->
    true.

add_host_key(_Host, _Port, _Key, _Options) ->
    ok.

load_magicbeam() ->
    case application:load(magicbeam) of
        ok ->
            ok;
        {error, {already_loaded, magicbeam}} ->
            ok
    end.

write_authorized_key(SshDir) ->
    {Public, Private} = crypto:generate_key(eddsa, ed25519),
    PrivateKey = #'ECPrivateKey'{
        version = 1,
        parameters = {namedCurve, ?'id-Ed25519'},
        privateKey = Private,
        publicKey = Public
    },
    PublicKey = ssh_file:extract_public_key(PrivateKey),
    AuthorizedKeys =
        ssh_file:encode(
            [{PublicKey, [{comment, "magicbeam-ct"}]}], auth_keys),
    ok = file:write_file(
        filename:join(SshDir, "authorized_keys"), AuthorizedKeys),
    PrivateKey.

send(Connection, Channel, Data) ->
    ssh_connection:send(Connection, Channel, Data, ?TIMEOUT).

send_keys(Connection, Channel, Keys) ->
    lists:foreach(
        fun(Key) -> ok = send(Connection, Channel, Key) end,
        Keys).

deadline() ->
    erlang:monotonic_time(millisecond) + ?TIMEOUT.

await_contains(Connection, Channel, Needle, Output, Deadline) ->
    case binary:match(Output, Needle) of
        nomatch ->
            receive_channel_data(
                Connection, Channel, Needle, Output, Deadline);
        _ ->
            Output
    end.

receive_channel_data(Connection, Channel, Needle, Output, Deadline) ->
    Remaining = max(1, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {ssh_cm, Connection, {data, Channel, Type, Data}}
          when Type =:= 0; Type =:= 1 ->
            await_contains(
                Connection, Channel, Needle,
                <<Output/binary, Data/binary>>, Deadline);
        {ssh_cm, Connection, {eof, Channel}} ->
            ct:fail({eof_waiting_for, Needle, Output});
        {ssh_cm, Connection, {closed, Channel}} ->
            ct:fail({closed_waiting_for, Needle, Output})
    after Remaining ->
        ct:fail({timeout_waiting_for, Needle, Output})
    end.

await_closed(Connection, Channel, Output, Deadline) ->
    Remaining = max(1, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {ssh_cm, Connection, {data, Channel, Type, Data}}
          when Type =:= 0; Type =:= 1 ->
            await_closed(
                Connection, Channel,
                <<Output/binary, Data/binary>>, Deadline);
        {ssh_cm, Connection, {eof, Channel}} ->
            await_closed(Connection, Channel, Output, Deadline);
        {ssh_cm, Connection, {exit_status, Channel, 0}} ->
            await_closed(Connection, Channel, Output, Deadline);
        {ssh_cm, Connection, {closed, Channel}} ->
            Output;
        {ssh_cm, Connection, {exit_status, Channel, Status}} ->
            ct:fail({unexpected_exit_status, Status, Output});
        {ssh_cm, Connection, {exit_signal, Channel, Signal, Error, _Lang}} ->
            ct:fail({unexpected_exit_signal, Signal, Error, Output})
    after Remaining ->
        ct:fail({timeout_waiting_for_close, Output})
    end.

assert_contains(Output, Needle) ->
    case binary:match(Output, Needle) of
        nomatch ->
            ct:fail({missing_output, Needle, Output});
        _ ->
            ok
    end.

assert_not_contains(Output, Needle) ->
    case binary:match(Output, Needle) of
        nomatch ->
            ok;
        _ ->
            ct:fail({unexpected_output, Needle, Output})
    end.
