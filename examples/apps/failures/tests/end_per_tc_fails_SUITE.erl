-module(end_per_tc_fails_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [a_test].

init_per_testcase(_TestcaseName, Config) ->
    Config.

end_per_testcase(_TestcaseName, _Config) ->
    assertion = bogus().

bogus() ->
    bogus.

a_test(_Config) ->
    ok.

