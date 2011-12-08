-module(magicbeam_app).
-author('jonafree@gmail.com').
-export([start/2, stop/1, rpc_start/1, rpc_stop/0]).

rpc_start(AppSpec) ->
    ok = application:load(AppSpec),
    ok = application:start(magicbeam).

rpc_stop() ->
    application:stop(magicbeam),
    application:unload(magicbeam),
    ok.

stop(_State) -> ok.

start(_Type, _Args) ->
    {ok, SupPid} = magicbeam_sup:start_link(),
    {ok, SupPid, {}}.
