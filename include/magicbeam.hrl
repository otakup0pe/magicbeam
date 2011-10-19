-include("magicbeam_log.hrl").
-include("magicbeam_config.hrl").

-include_lib("kernel/include/file.hrl").

-record(hotbeam, {mod, beam, src, beam_time=0, src_time=0, last=0}).
-record(hotbeam_state, {enable, src, apps=[], beams=[], tref=undefined, scantime=0, hotload_count=0, compile_count=0}). 

-record(thunderbeam_state, {enabled, tref=undefined, killed=0, base, variable, immune_app=[], immune_proc=[]}).

-define(enow(), calendar:datetime_to_gregorian_seconds(calendar:universal_time())).