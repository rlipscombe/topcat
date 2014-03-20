-module(topcat_slave).
-export([start/0]).

start() ->
    Hooks = [topcat_cth],
    [code:load_file(H) || H <- Hooks],

    {ok, [[TestDir]]} = init:get_argument(dir),
    {ok, [[LogDir]]} = init:get_argument(logdir),

    handle_make_result(topcat_make:dir(TestDir)),

    Opts = [
            {dir, filename:absname(TestDir)},
            {logdir, filename:absname(LogDir)},
            {ct_hooks, Hooks}],

    %% @todo Tidy this up
    handle_run_test_result(ct:run_test(Opts)).

handle_make_result(ok) ->
    ok;
handle_make_result(Error) ->
    topcat_server:notify({make_error, Error}),
    erlang:halt(1).

handle_run_test_result({_Ok, _Failed, {_UserSkipped, _AutoSkipped}}) ->
    erlang:halt(0);
handle_run_test_result({error, Reason}) ->
    topcat_server:notify({error, Reason}),
    erlang:halt(1).
