-module(tc_fails_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [this_fails].

this_fails(_Config) ->
    assertion = bogus().

bogus() ->
    bogus.

