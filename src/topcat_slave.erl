-module(topcat_slave).
-export([start/0]).

start() ->
    {topcat, 'topcat@localhost'} ! hello,

    % We want to redirect "user" output as soon as possible, otherwise CT gets a bit noisy.
    Hooks = [topcat_cth],
    [code:load_file(H) || H <- Hooks],

    TestDirs = filename:absname("apps/imp_server/suites"),
    LogDir = filename:absname("apps/imp_server/logs/ct"),
    Opts = [
            {dir, TestDirs},
            {logdir, LogDir},
            {ct_hooks, Hooks}],
    ct:run_test(Opts).
