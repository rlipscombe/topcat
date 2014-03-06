-module(topcat_slave).
-export([start/0]).

start() ->
    {topcat, 'topcat@localhost'} ! hello,
    Hooks = [topcat_cth],
    [code:load_file(H) || H <- Hooks],

    {ok, [[TestDir]]} = init:get_argument(dir),
    {ok, [[LogDir]]} = init:get_argument(logdir),

    Opts = [
            {dir, TestDir},
            {logdir, LogDir},
            {ct_hooks, Hooks}],
    ct:run_test(Opts).
