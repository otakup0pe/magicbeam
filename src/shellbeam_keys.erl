%% @author Jonathan Freedman
%% @copyright (c) 2012 ExactTarget, 2013-2026 Jonathan Freedman
%% @doc SSH server key callback module for magicbeam.
%%
%% Implements the ssh_server_key_api behaviour to provide Ed25519
%% host keys generated natively in Erlang, removing the dependency
%% on external ssh-keygen. Keys are optionally persisted to disk
%% for stability across restarts.
%% @end
-module(shellbeam_keys).
-behaviour(ssh_server_key_api).

-include("magicbeam.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([host_key/2, is_auth_key/3]).

%% @doc Return the host's Ed25519 private key.
%%
%% Attempts to load a persisted key from KeyDir. If no key exists
%% on disk, generates a new Ed25519 keypair and persists it.
%% The Algorithm argument is honored -- we only serve ed25519.
host_key('ssh-ed25519', Opts) ->
    KeyDir = key_dir(Opts),
    KeyFile = filename:join(KeyDir, "ssh_host_ed25519_key"),
    case load_host_key(KeyFile) of
        {ok, PrivKey} ->
            {ok, PrivKey};
        {error, _} ->
            case generate_and_persist(KeyFile) of
                {ok, PrivKey2} ->
                    ?info("generated new ed25519 host key in ~s", [KeyDir]),
                    {ok, PrivKey2};
                {error, Reason} ->
                    ?error("failed to generate host key: ~p", [Reason]),
                    {error, Reason}
            end
    end;
host_key(_Algorithm, _Opts) ->
    {error, unsupported_algorithm}.

%% @doc Check whether a given public key is authorized for the user.
%%
%% Reads authorized_keys from KeyDir (one key per line, standard
%% OpenSSH format) and checks if the connecting key matches any entry.
is_auth_key(PublicKey, User, Opts) ->
    KeyDir = key_dir(Opts),
    AuthFile = filename:join(KeyDir, "authorized_keys"),
    case file:read_file(AuthFile) of
        {ok, Bin} ->
            try ssh_file:decode(Bin, auth_keys) of
                AuthKeys when is_list(AuthKeys) ->
                    lists:any(
                      fun({Key, _Attrs}) -> Key == PublicKey end,
                      AuthKeys)
            catch
                _:_ -> false
            end;
        {error, Reason} ->
            ?warn("cannot read authorized_keys for ~p: ~p", [User, Reason]),
            false
    end.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @private Extract key_dir from ssh daemon options.
key_dir(Opts) ->
    Result = case proplists:get_value(key_cb_private, Opts) of
        undefined ->
            proplists:get_value(key_dir, Opts, "/tmp/shellbeam-ssh");
        PrivOpts when is_list(PrivOpts) ->
            proplists:get_value(key_dir, PrivOpts, "/tmp/shellbeam-ssh");
        _ ->
            "/tmp/shellbeam-ssh"
    end,
    Result.

%% @private Load a persisted Ed25519 private key from disk.
%% Stored as an Erlang binary term since ed25519 key tuples
%% do not have a standard PEM encoding in OTP's public_key.
load_host_key(KeyFile) ->
    case file:read_file(KeyFile) of
        {ok, Bin} ->
            try binary_to_term(Bin) of
                #'ECPrivateKey'{} = Key ->
                    {ok, Key};
                {ed_pri, ed25519, _Pub, _Priv} ->
                    ?info("found legacy ed_pri key, regenerating in new format", []),
                    file:delete(KeyFile),
                    {error, legacy_format};
                _Other ->
                    ?warn("unknown key format on disk, regenerating", []),
                    file:delete(KeyFile),
                    {error, unknown_format}
            catch
                _:Reason ->
                    {error, {decode_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Generate a new Ed25519 keypair and persist the private key.
generate_and_persist(KeyFile) ->
    try
        {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
        PrivKey = #'ECPrivateKey'{
            version = 1,
            parameters = {namedCurve, ?'id-Ed25519'},
            privateKey = Priv,
            publicKey = Pub
        },
        ok = filelib:ensure_dir(KeyFile),
        ok = file:write_file(KeyFile, term_to_binary(PrivKey)),
        ok = file:change_mode(KeyFile, 8#00600),
        {ok, PrivKey}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.
