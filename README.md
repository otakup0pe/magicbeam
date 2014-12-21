[![Build Status](https://travis-ci.org/otakup0pe/magicbeam.svg?branch=master)](https://travis-ci.org/otakup0pe/magicbeam)

                           _      _                          
     _ __ ___   __ _  __ _(_) ___| |__   ___  __ _ _ __ ___  
    | '_ ` _ \ / _` |/ _` | |/ __| '_ \ / _ \/ _` | '_ ` _ \ 
    | | | | | | (_| | (_| | | (__| |_) |  __/ (_| | | | | | |
    |_| |_| |_|\__,_|\__, |_|\___|_.__/ \___|\__,_|_| |_| |_|
                     |___/

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

In addition, magicbeam will (optionally) attempt to start a ssh server. This
server will only make use of public keys. No plans to support user/password
based authentication. Upon succesful connection a shellbeam REPL shellw ill
be spawned.

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
Magicbeam defaults to making use of OTP Application Environment for it's
configuration. If you wish to be fancy many options also support any 
configuration system you can throw at it via a hook module which implements a 
simple behavior. If you are not using a hook module and have not setup the 
application environment you may simply make use of application:set_env/3 and
magicbeam:rehash/0 to effect runtime changes.

Note that all these configuration options are in the magicbeam application.

| Key                          | Type          | Default
------------------------------ | ------------- | ----------------------------
thunderbeam_enabled            | bool          | `false`
thunderbeam_wait_base          | integer       | `60`
thunderbeam_wait_variable      | integer       | `5`
thunderbeam_immune_proc        | list of atoms | `[]`
thunderbeam_immune_app         | list of atoms | `[stdlib,kernel,mnesia,sasl,inets]`
thunderbeam_force_kill         | bool          | `false`
hotbeam_enabled                | bool          | `false`
hotbeam_compile                | bool          | `false`
hotbeam_apps                   | list of atoms | `[]`
callback                       | atom          | `undefined`
shellbeam_ansi                 | bool          | `false`
shellbeam_modules	           | list of atoms | `[magicbeam_shell]`
shellbeam_prompt	           | string/list   | `noname@nonode OTP4LYFE`
ssh			                   | bool          | `true`
ssh_port                       | integer       | `4422`
ssh_path                       | string/list   | *see note

For thunderbeam, the configuration is interpreted as follows
* processes are killed based on the rough formula of
  thunderbeam_wait_base * random:uniform(thunderbeam_wait_variable)
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
* Ansi coloring will take effect immediately however only works reliably
  from a ssh shell
* Prompt will take effect immediately

As the SSH server is loaded during execution of the OTP application callback
the configuration is not loaded via the hook. This must be specified as OTP
application environment config. The configuration is interpreted as follows
* Will only be started if set as true
* Will bind to the specified port
* If path is not set then it will attempt to use the priv directory followed
  by the users home directory followed by /tmp. The suffix '.magicbeam' will
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

Shoutouts
--------------------------------------------------------------------------------
Thanks to the following people / organizations
* Justin Kirby for inspiration on the k-rad means of creating an escript that
  injects beams into a running node
* Juan Jose Comellas for the getopt module
* Basho because rebar is awesome