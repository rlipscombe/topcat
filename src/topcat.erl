-module(topcat).
-export([main/1, halt/1]).

-define(TEMP_FOLDER, ".topcat").
-define(DEFAULT_CT_DIR, "tests").

main(_Args) ->
    topcat_archive:extract_beams(?TEMP_FOLDER),

    {ok, _} = topcat_server:start_link(),

    Applications = get_applications(),
    Result = run(Applications),
    topcat_server:stop(),
    case Result of
        ok -> topcat:halt(0);
        {exit_status, Status} -> topcat:halt(Status);
        _Other -> topcat:halt(1)
    end.

%% @doc Find applications (directories in apps that don't start with a dot).
%% @todo Use rebar.config instead?
get_applications() ->
    ["apps/" ++ A || A <- filelib:wildcard("*", "apps"), hd(A) =/= $.].

run([]) ->
    ok;
run([Application|Rest]) ->
    io:format("run ~s\n", [Application]),
    CtDir = get_ct_dir(Application),
    TestDir = filename:join(Application, CtDir),
    case filelib:is_dir(TestDir) of
        true ->
            case run_port(Application, CtDir) of
                ok -> run(Rest);
                Error -> Error
            end;
        _ ->
            io:format("Skipping ~s\n", [Application]),
            run(Rest)
    end.

run_port(Application, CtDir) ->
    SlaveArgs = create_slave_args(Application, CtDir),
    Cmd = "erl -noshell -noinput -sname topcat_child@localhost" ++
          SlaveArgs,
    io:format("~s\n", [Cmd]),
    Opts = [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    Port = erlang:open_port({spawn, Cmd}, Opts),
    port_loop(Port).

port_loop(Port) ->
    receive
        % We can occasionally receive 'exit_status' before the final 'data'.
        % Since we're only printing the output, we can safely ignore the
        % originator.
        {_Port, {data, {eol, Data}}} ->
            io:format("~s\n", [Data]),
            port_loop(Port);
        {_Port, {data, {noeol, Data}}} ->
            io:format("~s", [Data]),
            port_loop(Port);
        % 'exit_status' is sent when the process exits (but sometimes before
        % the final 'data').
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Status}} ->
            {exit_status, Status};
        % Otherwise, just print it out.
        Other ->
            io:format("~p\n", [Other]),
            port_loop(Port)
    end.

get_ct_dir(Application) ->
    RebarConfig = filename:join(Application, "rebar.config"),
    get_ct_dir_result(file:consult(RebarConfig)).

get_ct_dir_result({ok, Config}) ->
    get_ct_dir_from_config(Config);
get_ct_dir_result(_) ->
    ?DEFAULT_CT_DIR.

get_ct_dir_from_config([]) ->
    ?DEFAULT_CT_DIR;
get_ct_dir_from_config([{ct_dir, CtDir} | _Rest]) ->
    CtDir;
get_ct_dir_from_config([_ | Rest]) ->
    get_ct_dir_from_config(Rest).

arg_for(Switch, Value, Pred) ->
    arg_if(Switch, Value, Pred(Value)).

arg_if(Switch, Value, true) ->
    " " ++ Switch ++ " " ++ Value;
arg_if(_Switch, _Value, _) ->
    "".

get_config_file_path(Application, CtDir) ->
    filename:absname(filename:join([Application, CtDir, "app.config"])).

get_config_file_arg(Application, CtDir) ->
    arg_for("-config", get_config_file_path(Application, CtDir), fun filelib:is_regular/1).

get_include_dir_path(Application) ->
    filename:absname(filename:join([Application, "include"])).

get_include_arg(Application) ->
    arg_for("-include", get_include_dir_path(Application), fun filelib:is_dir/1).

create_slave_args(Application, CtDir) ->
    SlaveArgs = " -pa .topcat" ++
                " -pa " ++ create_code_paths("deps/*/ebin") ++
                " -pa " ++ create_code_paths("apps/*/ebin") ++
                " -s topcat_slave -s init stop" ++
                " -dir " ++ filename:join(Application, CtDir) ++
                " -logdir " ++ Application ++ "/logs/ct" ++
                " -env TEST_DIR " ++ Application ++
                get_include_arg(Application) ++
                get_config_file_arg(Application, CtDir),
    SlaveArgs.

%%% @todo ct_extra_params

create_code_paths(Wildcard) ->
    join_paths([filename:absname(P) || P <- filelib:wildcard(Wildcard)]).

join_paths(Paths) ->
    lists:foldl(fun(X, Acc) ->
                Acc ++ " " ++ X
        end, "", Paths).

halt(StatusCode) ->
    erlang:halt(StatusCode).
