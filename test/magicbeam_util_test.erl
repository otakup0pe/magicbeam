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

ssh_appenv_path_default_test() ->
    application:load(magicbeam),
    application:unset_env(magicbeam, ssh_path),
    ?assertEqual("/some/default/path",
                 magicbeam_util:ssh_appenv_path("/some/default/path")).

ssh_appenv_path_plain_list_override_test() ->
    application:load(magicbeam),
    application:set_env(magicbeam, ssh_path, "/explicit/path"),
    try
        ?assertEqual("/explicit/path",
                     magicbeam_util:ssh_appenv_path("/ignored/default"))
    after
        application:unset_env(magicbeam, ssh_path)
    end.

ssh_appenv_path_root_substitution_test() ->
    application:load(magicbeam),
    application:set_env(magicbeam, ssh_path, [root | "/etc/magicbeam-ssh"]),
    try
        Expected = code:root_dir() ++ "/etc/magicbeam-ssh",
        ?assertEqual(Expected,
                     magicbeam_util:ssh_appenv_path("/ignored/default"))
    after
        application:unset_env(magicbeam, ssh_path)
    end.
