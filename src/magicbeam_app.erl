-module(magicbeam_app).
-export([start/2, stop/1, rpc_start/1, rpc_stop/0, rehash/0]).

rpc_start(AppSpec) ->
    application:load(AppSpec),
    ok = application:start(magicbeam).

rpc_stop() ->
    ok = application:stop(magicbeam).

rehash() ->
    ok = thunderbeam:rehash(),
    ok = hotbeam:rehash().

stop(_State) -> ok.

start(_Type, _Args) ->
    {ok, SupPid} = magicbeam_sup:start_link(),
    {ok, SupPid, {}}.
