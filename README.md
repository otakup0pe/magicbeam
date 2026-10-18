                           _      _
     _ __ ___   __ _  __ _(_) ___| |__   ___  __ _ _ __ ___
    | '_ ` _ \ / _` |/ _` | |/ __| '_ \ / _ \/ _` | '_ ` _ \
    | | | | | | (_| | (_| | | (__| |_) |  __/ (_| | | | | | |
    |_| |_| |_|\__,_|\__, |_|\___|_.__/ \___|\__,_|_| |_| |_|
                     |___/

[![CI](https://github.com/otakup0pe/magicbeam/actions/workflows/test.yml/badge.svg)](https://github.com/otakup0pe/magicbeam/actions/workflows/test.yml)
[![Maintenance](https://img.shields.io/maintenance/yes/2026.svg)](https://github.com/otakup0pe/magicbeam)
[![Version](https://img.shields.io/badge/version-0.2.2-blue.svg)](https://github.com/otakup0pe/magicbeam/releases)
[![OTP](https://img.shields.io/badge/OTP-27%2B-4B275F.svg)](https://www.erlang.org/)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

Requires OTP 27 or later.

About
--------------------------------------------------------------------------------
Magicbeam contains three components which I have found useful when developing
fabulously robust Erlang/OTP systems.

* hotbeam handles the reloading of binary modules. optionally extracts source
  file location and monitors for changes in order to recompile and reload
  resulting beam
* thunderbeam will kill off random processes under the guises of testing
  supervisory structures
* shellbeam provides a simple OTP behaviour allowing one to easily define
  bespoke REPL shells for runtime diagnostics

In addition, magicbeam will optionally attempt to start an SSH server. This
server only supports public-key authentication; username/password
authentication is intentionally unsupported. Upon successful connection, a
shellbeam REPL shell will be spawned.

Installing
--------------------------------------------------------------------------------
Magicbeam can be installed either as a standalone escript or included as part
of a standard OTP release as a typical rebar dependency. Building magicbeam
itself will produce an includable OTP application and the escript.

Escript Usage
--------------------------------------------------------------------------------
Running magicbeam with no arguments will show the options. These should be
fairly self-explanatory. General procedure is to specify node and cookie with
load / unload to start / stop the magicbeam application. Note that this will
inject the application into an already running OTP system.

OTP Application Usage
--------------------------------------------------------------------------------
Include the magicbeam repository in your rebar-enabled project. It will be
started by the boot scripts when you start your node.

Configuration
--------------------------------------------------------------------------------
Magicbeam defaults to using the OTP application environment for its
configuration. Many options can instead use any configuration system exposed
through a hook module implementing the simple callback behaviour. Without a
hook module, use application:set_env/3 and magicbeam:rehash/0 to apply runtime
changes.

Note that all these configuration options are in the magicbeam application.

| Key                          | Type          | Default
------------------------------ | ------------- | ----------------------------
thunderbeam_enabled            | bool          | `false`
thunderbeam_wait_base          | integer       | `60`
thunderbeam_wait_variable      | integer       | `5`
thunderbeam_kill_attempts      | integer       | `10`
thunderbeam_immune_proc        | list of atoms | `[]`
thunderbeam_immune_app         | list of atoms | `[stdlib,kernel,mnesia,sasl,inets]`
thunderbeam_force_kill         | bool          | `false`
thunderbeam_force_app          | list of atoms | `[]`
hotbeam_enabled                | bool          | `false`
hotbeam_compile                | bool          | `false`
hotbeam_apps                   | list of atoms | `[]`
callback                       | atom          | `undefined`
shellbeam_ansi                 | bool          | `false`
shellbeam_modules              | list of atoms | `[magicbeam_shell]`
shellbeam_prompt               | string/list   | `<node()> magicbeam shell`
shell_mfa                      | `{M,F,A}`     | `undefined` (see note)
ssh                            | bool          | `true`
ssh_port                       | integer       | `4422`
ssh_path                       | string/list   | *see note

For thunderbeam, the configuration is interpreted as follows
* processes are killed based on the rough formula of
  thunderbeam_wait_base * rand:uniform(thunderbeam_wait_variable)
* if a process has a registered name in thunderbeam_immune_proc it will
  never be killed. in addition, the application_master process will never be
  killed
* if a process is considered part of an application in
  thunderbeam_immune_app it will never be killed
* if sending the 'seppuku' atom as a message to a process with the trap_exit
  flag set does not cause it to quit and the thunderbeam_force_kill config
  variable is set to true then it will be sent a 'kill' exit signal

For hotbeam the configuration is interpreted as follows
* Source files are only monitored if hotbeam_compile is true
* Only modules in the applications listed in hotbeam_apps are monitored

For shellbeam the configuration is interpreted as follows
* Specified modules are used to determine loaded commands for top level shell
  and changes are not detected until shell is restarted
* ANSI coloring takes effect immediately but only works reliably
  from an SSH shell
* Prompt will take effect immediately
* `shellbeam:start_shell/3` accepts an options map; `#{banner => Text}`
  replaces the default Magicbeam banner. `start_shell/2` retains the default.
* `shell_mfa`, if set, overrides the default SSH shell entry point. The MFA
  should eventually call `shellbeam:start_shell/2` or `start_shell/3` with
  whatever modules, prompt, and presentation the caller wants. This lets
  downstream applications route SSH shells to their own setup code without
  replacing shellbeam. When unset, shellbeam uses `shellbeam_modules` +
  `shellbeam_prompt` directly.

As the SSH server is loaded during execution of the OTP application callback
the configuration is not loaded via the hook. This must be specified as OTP
application environment config. The configuration is interpreted as follows
* Will only be started if set as true
* Will bind to the specified port
* If path is not set then it will attempt to use the priv directory followed
  by the user's home directory followed by /tmp. The suffix '.magicbeam' will
  be appended to any automatically interpreted path. If the host keys do not
  exist then they will be created. Be sure to create an authorized_keys file!

Integration
--------------------------------------------------------------------------------
There are two optional integration points for existing applications. The
callback hook module is specified either by the 'callback' application
environment variable or on the command line when remotely injecting via the
escript. In addition, you may include the helpers.hrl file and make use of the
magicbeam_handle_info macro to implement a gen_server:handle_info/2 callback
function. This will cause a gen_server which has the trap_exit flag set to
terminate upon receipt of the single atom 'seppuku' with a non-normal reason.

The hook should implement the 'magicbeam' behaviour. In lieu of edocs the
API is roughly as follows.

Callback:init() -> {ok, State}
--------------------
This function is meant to perform whatever initialization your hook needs. It
returns an opaque variable which will be passed into other callback functions.

Callback:event(Term, State) -> ok
--------------------
This function is called when magicbeam performs actions. In case you care as
much about logging and metrics as I do.

Callback:config(Key, Default, State) -> undefined | {ok, Value}
--------------------
This function is used to override the default usage of application environment
for configuration.

Callback:terminate(State) -> ok
--------------------
Called when the magicbeam application is shutting down.

Releases
--------------------------------------------------------------------------------
Releases are cut automatically after CI passes on `mainline`. Include one of
`#patch`, `#minor`, or `#major` in a commit message (the equivalent
`bump:patch`, `bump:minor`, and `bump:major` forms also work). If several
hints occur since the last release, the largest change wins. Release tags are
unprefixed semantic versions such as `0.2.0`.

Note on AI Usage
--------------------------------------------------------------------------------
This project has been developed with AI assistance. Contributions making
use of AI generated content are welcome, however they _must_ be human
reviewed prior to submission as pull requests, or issues. All contributors
must be able to fully explain and defend any AI generated code,
documentation, issues, or tests they submit. Contributions making use of AI
must have this explicitly declared in the pull request or issue. This
also applies to utilization of AI for reviewing of pull requests.

Shoutouts
--------------------------------------------------------------------------------
Thanks to the following people / organizations
* Justin Kirby for inspiration on the k-rad means of creating an escript that
  injects beams into a running node
* Juan Jose Comellas for the getopt module
* Basho because rebar is awesome
