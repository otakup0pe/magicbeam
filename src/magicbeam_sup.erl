-module(magicbeams_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3, 10}, [
        {thunderbeam, {thunderbeam, start_link, []}, permanent, 2000, worker, [gen_server]},
        {hotbeam, {hotbeam, start_link, []}, permanent, 2000, worker, [gen_server]}
    ]}}.