-module(init_per_tc_fails_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [should_be_skipped].

init_per_testcase(_TestcaseName, _Config) ->
    assertion = bogus().

bogus() ->
    bogus.

should_be_skipped(_Config) ->
    ok.

