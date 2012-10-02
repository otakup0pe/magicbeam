-module(thunder_gen_server).

-export([start_link/3, start_link/4, call/2, call/3, cast/2]).
-include("magicbeam.hrl").

start_link(CB, Opaque, Args) ->
    wait(),
    gen_server:start_link(CB, Opaque, Args).

start_link(Name, CB, Opaque, Args) ->
    wait(),
    gen_server:start_link(Name, CB, Opaque, Args).

call(N, M) ->
    wait(),
    gen_server:call(N, M).

call(N, M, T) ->
    wait(),
    gen_server:call(N, M, T).

cast(N, M) ->
    wait(),
    gen_server:cast(N, M).

wait() ->
    magicbeam_util:random(?THUNDER_GS_MIN, ?THUNDER_GS_MAX).
