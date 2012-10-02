-module(magicbeam_util_test).

-include_lib("eunit/include/eunit.hrl").

random_test() ->
    A = magicbeam_util:random(5, 20),
    ?assert(A >= 5),
    ?assert(A =< 20).

random_base_test() ->
    A = magicbeam_util:random(5, 20, 5),
    ?assert(A >= 10),
    ?assert(A =< 25).
