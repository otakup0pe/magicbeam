-include("thunderbeam_log.hrl").

-define(KILLER_ENABLE_DEFAULT, false).
-define(KILLER_FREQUENCY_BASE_DEFAULT, 60).
-define(KILLER_FREQUENCY_VARIABLE_DEFAULT, 5).

-define(KILLER_IMMUNE_PROC_DEFAULT, []).
-define(KILLER_IMMUNE_APP_DEFAULT, [stdlib, kernel, mnesia, sasl, inets]).

-define(KILLER_FREQUENCY_BASE, thunderbeam_util:app_env(killer_frequency_base, ?KILLER_FREQUENCY_BASE_DEFAULT)).
-define(KILLER_FREQUENCY_VARIABLE, thunderbeam_util:app_env(killer_frequency_variable, ?KILLER_FREQUENCY_VARIABLE_DEFAULT)).
