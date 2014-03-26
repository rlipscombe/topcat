-module(end_per_tc_fails_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [a_test].

init_per_testcase(_TestcaseName, Config) ->
    Config.

end_per_testcase(_TestcaseName, _Config) ->
    %% Note that this failure is COMPLETELY eaten by Common Test. topcat_cth
    %% doesn't get to see it either.
    assertion = bogus().

bogus() ->
    bogus.

a_test(_Config) ->
    ok.

