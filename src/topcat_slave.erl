-module(topcat_slave).
-export([start/0]).

start() ->
    % Note that we expect to be started IN the application directory.
    Hooks = [topcat_cth],
    [code:load_file(H) || H <- Hooks],

    {ok, [[TestDir]]} = init:get_argument(dir),
    {ok, [[LogDir]]} = init:get_argument(logdir),
    
    CoverOpts = case init:get_argument(cover) of
        {ok, [[CoverSpec]]} -> get_cover_opts(CoverSpec, LogDir);
        _ -> []
    end,

    handle_make_result(topcat_make:dir(TestDir)),

    ok = filelib:ensure_dir(filename:join(LogDir, "DUMMY")),

    Opts = [{dir, filename:absname(TestDir)},
            {logdir, filename:absname(LogDir)},
            {ct_hooks, Hooks},
            {auto_compile, true}] ++ CoverOpts,

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
