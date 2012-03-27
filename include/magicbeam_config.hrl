-define(THUNDERBEAM_KILL_ATTEMPTS, 2).
-define(THUNDERBEAM_NEVER_KILL_PROC, [application_master]).
-define(THUNDERBEAM_ENABLED, magicbeam_srv:cfgget(thunderbeam_enabled, false)).
-define(THUNDERBEAM_WAIT_BASE, magicbeam_srv:cfgget(thunderbeam_wait_base, 60)).
-define(THUNDERBEAM_WAIT_VARIABLE, magicbeam_srv:cfgget(thunderbeam_wait_variable, 5)).
-define(THUNDERBEAM_IMMUNE_PROC, ?THUNDERBEAM_NEVER_KILL_PROC ++ magicbeam_srv:cfgget(thunderbeam_immune_proc, [])).
-define(THUNDERBEAM_IMMUNE_APP, magicbeam_srv:cfgget(thunderbeam_immune_app, [stdlib,kernel,mnesia,sasl,inets])).
-define(THUNDERBEAM_FORCE_KILL, magicbeam_srv:cfgget(thunderbeam_force_kill, false)).
-define(THUNDERBEAM_FORCE_APPS, magicbeam_srv:cfgget(thunderbeam_force_app, [])).

-define(HOTBEAM_ENABLED, magicbeam_srv:cfgget(hotbeam_enabled, false)).
-define(HOTBEAM_COMPILE, magicbeam_srv:cfgget(hotbeam_compile, false)).
-define(HOTBEAM_APPS, magicbeam_srv:cfgget(hotbeam_apps, [])).
-define(HOTBEAM_LOOP, 1000).

-define(SHELLBEAM_ANSI, magicbeam_srv:cfgget(shellbeam_ansi, false)).
-define(SHELLBEAM_PROMPT, magicbeam_srv:cfgget(shellbeam_prompt, atom_to_list(node()) ++ " magicbeam shell")).
-define(SHELLBEAM_MODULES, magicbeam_srv:cfgget(shellbeam_modules, [magicbeam_shell])).
