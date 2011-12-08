-module(magicbeam_sup).
-author('jonafree@gmail.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3, 10}, [
        {magicbeam_srv, {magicbeam_srv, start_link, []}, permanent, 10000, worker, [gen_server]},
        {thunderbeam, {thunderbeam, start_link, []}, permanent, 2000, worker, [gen_server]},
        {hotbeam, {hotbeam, start_link, []}, permanent, 2000, worker, [gen_server]}
    ]}}.
