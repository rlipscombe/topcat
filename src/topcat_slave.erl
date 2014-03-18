-module(topcat_slave).
-export([start/0]).

start() ->
    Hooks = [topcat_cth],
    [code:load_file(H) || H <- Hooks],

    {ok, [[TestDir]]} = init:get_argument(dir),
    {ok, [[LogDir]]} = init:get_argument(logdir),

    %% @todo Tidy this up.
    %% @todo Also capture the output?
    case topcat_make:dir(TestDir) of
        ok ->
            ok;
        Error ->
            io:format("make = ~p\n", [Error]),
            erlang:halt(1)
    end,

    Opts = [
            {dir, filename:absname(TestDir)},
            {logdir, filename:absname(LogDir)},
            {ct_hooks, Hooks}],

    %% @todo Tidy this up
    case ct:run_test(Opts) of
        {_Ok, _Failed, {_UserSkipped, _AutoSkipped}} ->
            erlang:halt(0);     % ??
        {error, Reason} ->
            io:format("run_test = ~p\n", [Reason]),
            erlang:halt(1);
        _RunnerPid ->
            erlang:error(unexpected)
    end.
