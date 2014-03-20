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
            {ct_hooks, Hooks},
            {auto_compile, false}],

    %% @todo Tidy this up
    handle_run_test_result(ct:run_test(Opts)).

handle_make_result(ok) ->
    ok;
handle_make_result(Results) ->
    topcat_server:notify({make_error, Results}),
    lists:foreach(fun check_make_result/1, Results).

check_make_result({ok, _}) -> ok;
check_make_result({ok, _, _}) -> ok;
check_make_result({ok, _, _, _}) -> ok;
check_make_result(_) -> topcat:halt(1).

handle_run_test_result({_Ok, _Failed, {_UserSkipped, _AutoSkipped}}) ->
    topcat:halt(0);
handle_run_test_result({error, Reason}) ->
    topcat_server:notify({error, Reason}),
    topcat:halt(1).
