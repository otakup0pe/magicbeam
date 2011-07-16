-module(thunderbeam_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3, 10}, [
        {nomura_test_killer, {nomura_test_killer, start_link, []}, permanent, 2000, worker, [gen_server]}
    ]}}.
