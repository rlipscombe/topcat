-module(fail_SUITE).
-compile(export_all).

all() -> [
    this_should_fail
].

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

this_should_fail(_Config) ->
    true = this().

this() ->
    false.

