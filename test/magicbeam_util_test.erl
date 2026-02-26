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

random_repeated_test() ->
    %% rand module self-seeds; verify repeated calls work without explicit seeding
    Results = [magicbeam_util:random(1, 100) || _ <- lists:seq(1, 50)],
    lists:foreach(fun(R) ->
        ?assert(R >= 1),
        ?assert(R =< 100)
    end, Results).

random_min_equals_max_test() ->
    ?assertEqual(10, magicbeam_util:random(10, 10)),
    ?assertEqual(15, magicbeam_util:random(10, 10, 5)).

appenv_default_test() ->
    ?assertEqual(default_val, magicbeam_util:appenv(nonexistent_key_test, default_val)).

appenv_set_test() ->
    application:load(magicbeam),
    application:set_env(magicbeam, test_key_12345, some_value),
    ?assertEqual(some_value, magicbeam_util:appenv(test_key_12345, other)),
    application:unset_env(magicbeam, test_key_12345).
