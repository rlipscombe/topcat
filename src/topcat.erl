-module(topcat).
-export([main/1]).

-define(TEMP_FOLDER, ".topcat").
-define(SUITES, "suites").

main(_Args) ->
    topcat_archive:extract_beams(?TEMP_FOLDER),

    {ok, _} = topcat_server:start_link(),

    Applications = get_applications(),
    Result = run(Applications),
    topcat_server:stop(),
    case Result of
        ok -> erlang:halt(0);
        {exit_status, Status} -> erlang:halt(Status);
        _Other -> erlang:halt(1)
    end.

%% @doc Find applications (directories in apps that don't start with a dot).
%% @todo Use rebar.config instead?
get_applications() ->
    ["apps/" ++ A || A <- filelib:wildcard("*", "apps"), hd(A) =/= $.].

run([]) ->
    ok;
run([Application|Rest]) ->
    case run_port(Application) of
        ok -> run(Rest);
        Error -> Error
    end.

run_port(Application) ->
    SlaveArgs = create_slave_args(Application),
    Cmd = "erl -noshell -noinput -sname topcat_child@localhost" ++
          SlaveArgs,
    io:format("~s\n", [Cmd]),
    Opts = [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    Port = erlang:open_port({spawn, Cmd}, Opts),
    port_loop(Port).

port_loop(Port) ->
    receive
        {Port, {data, {eol, Data}}} ->
            io:format("~s~n", [Data]),
            port_loop(Port);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Status}} ->
            {exit_status, Status};
        Other ->
            io:format("~p~n", [Other]),
            port_loop(Port)
    end.

arg_for(Switch, Value, Pred) ->
    arg_if(Switch, Value, Pred(Value)).

arg_if(Switch, Value, true) ->
    " " ++ Switch ++ " " ++ Value;
arg_if(_Switch, _Value, _) ->
    "".

get_config_file_path(Application) ->
    filename:absname(filename:join([Application, ?SUITES, "app.config"])).

get_config_file_arg(Application) ->
    arg_for("-config", get_config_file_path(Application), fun filelib:is_regular/1).

get_include_dir_path(Application) ->
    filename:absname(filename:join([Application, "include"])).

get_include_arg(Application) ->
    arg_for("-include", get_include_dir_path(Application), fun filelib:is_dir/1).

create_slave_args(Application) ->
    SlaveArgs = " -pa .topcat" ++
                " -pa " ++ create_code_paths("deps/*/ebin") ++
                " -pa " ++ create_code_paths("apps/*/ebin") ++
                " -s topcat_slave -s init stop" ++
                " -dir " ++ filename:join(Application, ?SUITES) ++
                " -logdir " ++ Application ++ "/logs/ct" ++
                " -env TEST_DIR " ++ Application ++
                get_include_arg(Application) ++
                get_config_file_arg(Application),
    SlaveArgs.

create_code_paths(Wildcard) ->
    join_paths([filename:absname(P) || P <- filelib:wildcard(Wildcard)]).

join_paths(Paths) ->
    lists:foldl(fun(X, Acc) ->
                Acc ++ " " ++ X
        end, "", Paths).
