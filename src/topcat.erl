-module(topcat).
-export([main/1]).

-define(TEMP_FOLDER, ".topcat").

main(_Args) ->
    io:format("topcat~n"),
    topcat_archive:extract_beams(?TEMP_FOLDER),

    % @todo Don't register self; that results in mixing the receive loop below.
    register(topcat, self()),
    %run_slave().
    run_port().

run_port() ->
    SlaveArgs = create_slave_args(),
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
        {Port, {exit_status, Status}} ->
            ok;
        Other ->
            io:format("~p~n", [Other]),
            port_loop(Port)
    end.

create_slave_args() ->
    SlaveArgs = " -pa .topcat" ++
                " -pa " ++ lists:foldl(fun(X, Acc) -> Acc ++ " " ++ X end, "", filelib:wildcard("deps/*/ebin")) ++
                " -pa " ++ lists:foldl(fun(X, Acc) -> Acc ++ " " ++ X end, "", filelib:wildcard("apps/*/ebin")) ++
                " -s topcat_slave -s init stop"
                " -config /home/roger/Source/imp/imp_server/apps/imp_server/suites/app.config",
    io:format("~p~n", [SlaveArgs]),
    SlaveArgs.

run_slave() ->
    SlaveArgs = create_slave_args(),
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
