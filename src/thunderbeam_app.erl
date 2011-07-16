-module(thunderbeam_app).
-export([start/2, stop/1]).

stop(_State) -> ok.

start(_Type, _Args) ->
    {ok, SupPid} = thunderbeam_sup:start_link(),
    {ok, SupPid, {}}.
