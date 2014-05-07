-module(topcat_slave).
-export([start/0]).

%% @doc Entry point.
start() ->
    % Note that we expect to be started IN the application directory.
    Hooks = [topcat_cth],
    [code:load_file(H) || H <- Hooks],

    {ok, [[TestDir]]} = init:get_argument(dir),
    {ok, [[LogDir]]} = init:get_argument(logdir),
    
    CoverOpts = get_cover_opts(),
    FilterOpts = get_filter_opts(),

    handle_make_result(topcat_make:dir(TestDir)),

    ok = filelib:ensure_dir(filename:join(LogDir, "DUMMY")),

    Opts = [{dir, filename:absname(TestDir)},
            {logdir, filename:absname(LogDir)},
            {ct_hooks, Hooks},
            {auto_compile, false}] ++ FilterOpts ++ CoverOpts,

    error_logger:delete_report_handler(error_logger_tty_h),
    error_logger:add_report_handler(error_logger_topcat_h),

    handle_run_test_result(ct:run_test(Opts)),
    CoverDataFile = get_cover_data_file(CoverOpts),
    print_coverage(CoverDataFile).

get_cover_data_file([]) ->
    undefined;
get_cover_data_file([{cover, CoverSpec}]) ->
    {ok, Specs} = file:consult(CoverSpec),
    proplists:get_value(export, Specs, undefined).

print_coverage(undefined) ->
    ok;
print_coverage(CoverDataFile) ->
    ok = cover:import(CoverDataFile),
    lists:foreach(
        fun(Module) ->
                {ok, {Module, {Cov, NotCov}}} = cover:analyse(Module, module),
                topcat_server:notify({coverage, {Module, {Cov, NotCov}}})
        end, cover:imported_modules()).

get_temp_cover_spec(LogDir) ->
    filename:absname(filename:join(LogDir, ".cover.spec")).

get_filter_opts() ->
    get_filter_opts(suite, suite) ++
    get_filter_opts(group, group) ++
    get_filter_opts('case', testcase).

get_filter_opts(Flag, Key) ->
    case init:get_argument(Flag) of
        {ok, [Values]} -> [{Key, [list_to_atom(V) || V <- Values]}];
        _ -> []
    end.

get_cover_opts() ->
    {ok, [[LogDir]]} = init:get_argument(logdir),
    case init:get_argument(cover) of
        {ok, [[CoverSpec]]} -> get_cover_opts(CoverSpec, LogDir);
        _ -> []
    end.

get_cover_opts(CoverSpec, LogDir) ->
    TempCoverSpec = get_temp_cover_spec(LogDir),
    topcat_cover:tidy_spec(CoverSpec, TempCoverSpec),
    [{cover, filename:absname(TempCoverSpec)}].

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
    ok;
handle_run_test_result({error, Reason}) ->
    topcat_server:notify({error, Reason}),
    topcat:halt(1).

