-define(THUNDERBEAM_ENABLED, magicbeam_srv:cfgget(thunderbeam_enabled, false)).
-define(THUNDERBEAM_WAIT_BASE, magicbeam_srv:cfgget(thunderbeam_wait_base, 60)).
-define(THUNDERBEAM_WAIT_VARIABLE, magicbeam_srv:cfgget(thunderbeam_wait_variable, 5)).
-define(THUNDERBEAM_IMMUNE_PROC, magicbeam_srv:cfgget(thunderbeam_immune_proc, "")).
-define(THUNDERBEAM_IMMUNE_APP, magicbeam_srv:cfgget(thunderbeam_immune_app, "stdlib,kernel,mnesia,sasl,inets")).

-define(HOTBEAM_ENABLED, magicbeam_srv:cfgget(hotbeam_enabled, true)).
-define(HOTBEAM_COMPILE, magicbeam_srv:cfgget(hotbeam_compile, false)).
-define(HOTBEAM_APPS, magicbeam_srv:cfgget(hotbeam_apps, [])).
-define(HOTBEAM_LOOP, 1000).
