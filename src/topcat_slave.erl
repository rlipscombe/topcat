-module(topcat_slave).
-export([start/0]).

start() ->
    Hooks = [topcat_cth],
    [code:load_file(H) || H <- Hooks],

    {ok, [[TestDir]]} = init:get_argument(dir),
    {ok, [[LogDir]]} = init:get_argument(logdir),

    Opts = [
            {dir, filename:absname(TestDir)},
            {logdir, filename:absname(LogDir)},
            {ct_hooks, Hooks}],
    ct:run_test(Opts).
