-module(topcat).
-export([main/1, halt/1]).

-define(TEMP_FOLDER, ".topcat").

main(Argv) ->
    Args = topcat_args:parse_args(Argv),
    [application:set_env(topcat, P, V) || {P, V} <- Args],

    AppFilter = topcat_args:get_all_arguments(app, Args),

    SuiteFilter = topcat_args:get_all_arguments(suite, Args),
    GroupFilter = topcat_args:get_all_arguments(group, Args),
    TestCaseFilter = topcat_args:get_all_arguments('case', Args),
    Opts = [{suite, SuiteFilter}, {group, GroupFilter}, {'case', TestCaseFilter}],

    topcat_archive:extract_beams(?TEMP_FOLDER),

    {ok, _} = topcat_server:start_link(),

    Applications = get_applications(AppFilter),
    Result = run(Applications, Opts),
    Summary = topcat_server:stop(),
    halt_for(Result, Summary).

halt_for(ok, {_OK, 0, 0}) ->
    topcat:halt(0);
halt_for(ok, {_OK, _Skipped, _Failed}) ->
    topcat:halt(1);
halt_for({exit_status, Status}, _Summary) ->
    topcat:halt(Status);
halt_for(_Other, _Summary) ->
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
            io:format("~s not found; skipping ~s\n", [TestDir, Application]),
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
    topcat_port:run(Cmd, PortOpts).

halt(StatusCode) ->
    erlang:halt(StatusCode).
