-module(compile_fails_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [the_test].

the_test() ->
    ok = broken().

broken() ->
    ok;
broken(X) ->
    head_mismatch.
