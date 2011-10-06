-include("magicbeam_log.hrl").
-include("magicbeam_config.hrl").

-include_lib("kernel/include/file.hrl").

-record(hotbeam_state, {enabled, apps=[], modtimes=[], tref=undefined}). 
-record(thunderbeam_state, {enabled, tref=undefined, killed=0, base, variable, immune_app=[], immune_proc=[]}).

-define(enow(), calendar:datetime_to_gregorian_seconds(calendar:universal_time())).