-module(topcat).
-export([main/1, halt/1]).

-define(TEMP_FOLDER, ".topcat").

main(Argv) ->
    Args = topcat_args:parse_args(Argv),
    AppFilter = topcat_args:get_all_arguments(app, Args),
    
    SuiteFilter = topcat_args:get_all_arguments(suite, Args),
    GroupFilter = topcat_args:get_all_arguments(group, Args),
    TestCaseFilter = topcat_args:get_all_arguments('case', Args),
    Opts = [{suite, SuiteFilter}, {group, GroupFilter}, {'case', TestCaseFilter}],

    topcat_archive:extract_beams(?TEMP_FOLDER),

    {ok, _} = topcat_server:start_link(),

    Applications = get_applications(AppFilter),
    Result = run(Applications, Opts),
    topcat_server:stop(),
    halt_for(Result).

halt_for(ok) ->
    topcat:halt(0);
halt_for({exit_status, Status}) ->
    topcat:halt(Status);
halt_for(_Other) ->
    topcat:halt(1).

%% @doc Find applications (directories in apps that don't start with a dot).
%% @todo Use rebar.config instead, and look for 'sub_dirs'.
get_applications([]) ->
    ["apps/" ++ A || A <- filelib:wildcard("*", "apps"), hd(A) =/= $.];
get_applications(Filter) ->
    ["apps/" ++ A || A <- filelib:wildcard("*", "apps"), lists:member(A, Filter)].

run([], _Opts) ->
    ok;
run([Application|Rest], Opts) ->
    io:format("run ~s\n", [Application]),
    case run_suites(Application, Opts) of
        ok -> run(Rest, Opts);
        Error -> Error
    end.

run_suites(Application, Opts) ->
    Config = topcat_config:get_config(Application),
    CtDir = topcat_config:get_ct_dir(Config),
    CoverEnabled = topcat_config:get_cover_enabled(Config),

    TestDir = filename:join(Application, CtDir),
    case filelib:is_dir(TestDir) of
        true ->
            run_port(Application, Opts, CtDir, CoverEnabled);
        _ ->
            io:format("Skipping ~s\n", [Application]),
            ok
    end.

run_port(Application, Opts, CtDir, CoverEnabled) ->
    SlaveArgs = topcat_slave_args:create_slave_args(Application, Opts, CtDir, CoverEnabled),
    Cmd = "erl -noshell -noinput -sname topcat_child@localhost" ++
          SlaveArgs,
    io:format("~s\n", [Cmd]),
    PortOpts = [exit_status,
            {line, 16384}, use_stdio, stderr_to_stdout, hide,
            {cd, Application}],
    Port = erlang:open_port({spawn, Cmd}, PortOpts),
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

halt(StatusCode) ->
    erlang:halt(StatusCode).
