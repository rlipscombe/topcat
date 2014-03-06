-module(topcat).
-export([main/1]).

-define(TEMP_FOLDER, ".topcat").

main(_Args) ->
    io:format("topcat~n"),
    topcat_archive:extract_beams(?TEMP_FOLDER),

    {ok, _} = topcat_server:start_link(),

    Applications = get_applications(),
    run(Applications),
    topcat_server:stop().

%% @doc Find applications (directories in apps that don't start with a dot).
%% @todo Use rebar.config instead?
get_applications() ->
    ["apps/" ++ A || A <- filelib:wildcard("*", "apps"), hd(A) =/= $.].

run([]) ->
    ok;
run([Application|Rest]) ->
    %run_slave().
    run_port(Application),
    run(Rest).

run_port(Application) ->
    SlaveArgs = create_slave_args(Application),
    Cmd = "erl -noshell -noinput -sname topcat_child@localhost" ++
    SlaveArgs,
    Opts = [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    Port = erlang:open_port({spawn, Cmd}, Opts),
    port_loop(Port).

port_loop(Port) ->
    receive
        {Port, {data, {eol, Data}}} ->
            io:format("~s~n", [Data]),
            port_loop(Port);
        {Port, {exit_status, _Status}} ->
            ok;
        Other ->
            io:format("~p~n", [Other]),
            port_loop(Port)
    end.

get_config_file_arg(Application) ->
    ConfigFile = Application ++ "/suites/app.config",
    case filelib:is_regular(ConfigFile) of
        true -> " -config " ++ ConfigFile;
        _ -> ""
    end.

create_slave_args(Application) ->
    SlaveArgs = " -pa .topcat" ++
    " -pa " ++ lists:foldl(fun(X, Acc) -> Acc ++ " " ++ X end, "", filelib:wildcard("deps/*/ebin")) ++
    " -pa " ++ lists:foldl(fun(X, Acc) -> Acc ++ " " ++ X end, "", filelib:wildcard("apps/*/ebin")) ++
    " -s topcat_slave -s init stop" ++
    " -dir " ++ Application ++ "/suites" ++
    " -logdir " ++ Application ++ "/logs/ct" ++
    " -env TEST_DIR " ++ Application ++
    get_config_file_arg(Application),
    io:format("SlaveArgs=~p~n", [SlaveArgs]),
    SlaveArgs.

run_slave(Application) ->
    SlaveArgs = create_slave_args(Application),
    {ok, Node} = slave:start_link(localhost, topcat_child, SlaveArgs),
    monitor_node(Node, true),
    monitor_loop(Node).

monitor_loop(Node) ->
    receive
        {nodedown,Node} ->
            io:format("Slave died; we're done here.~n"),
            ok;
        Other ->
            io:format("~p~n", [Other]),
            monitor_loop(Node)
    end.

%    Ok = proplists:get_value(ok, Status, []),
%    [log_ok_result("OK: ~p~n", [TestcaseName]) || {_SuiteName, TestcaseName} <- Ok],
%    Skipped = proplists:get_value(skipped, Status, []),
%    [log_skipped_result("Skipped: ~p~n", [TestcaseName]) || {_SuiteName, TestcaseName} <- Skipped],
%    Failed = proplists:get_value(failed, Status, []),
%    [log_failed_result("Failed: ~p~n", [TestcaseName]) || {_SuiteName, TestcaseName} <- Failed],
    
    %log_verbose("post_end_per_suite(SuiteName=~p, Config=~p, Return=~p, State=~p~n",
    %          [SuiteName, Config, Return, State]),

