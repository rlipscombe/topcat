-module(topcat_slave_args).
-export([create_slave_args/4]).

create_slave_args(Application, Opts, CtDir, CoverEnabled) ->
    LogDir = "logs/ct",
    TestDir = get_path_for(Application, CtDir),

    SlaveArgs = " -pa " ++ create_code_paths("deps/*/ebin") ++
                " -pa " ++ create_code_paths("apps/*/ebin") ++
                " -pa " ++ create_code_paths("_build/*/lib/*/ebin") ++
                " -pa " ++ filename:absname(".topcat") ++
                " -s topcat_slave -s init stop" ++
                " -dir " ++ TestDir ++
                get_suite_arg(Opts) ++
                get_group_arg(Opts) ++
                get_case_arg(Opts) ++
                " -logdir " ++ get_path_for(Application, LogDir) ++
                get_cover_arg(Application, CoverEnabled) ++
                " -env TEST_DIR " ++ TestDir ++
                get_include_arg(Application) ++
                get_config_file_arg(Application, CtDir),
    SlaveArgs.

get_suite_arg(Opts) ->
    get_opt_arg(suite, "-suite", Opts).

get_group_arg(Opts) ->
    get_opt_arg(group, "-group", Opts).

get_case_arg(Opts) ->
    get_opt_arg('case', "-case", Opts).

get_opt_arg(Key, Switch, Opts) ->
    case proplists:get_value(Key, Opts) of
        undefined -> "";
        [] -> "";
        Values -> " " ++ Switch ++ " " ++ string:join(Values, " ")
    end.

%%% @todo ct_extra_params

create_code_paths(Wildcard) ->
    join_paths([filename:absname(P) || P <- filelib:wildcard(Wildcard)]).

join_paths(Paths) ->
    lists:foldl(fun(X, Acc) ->
                Acc ++ " " ++ X
        end, "", Paths).

get_cover_arg(Application, true) ->
    arg_for("-cover", get_cover_spec_path(Application), fun filelib:is_regular/1);
get_cover_arg(_Application, false) ->
    "".

get_cover_spec_path(Application) ->
    get_path_for(Application, "cover.spec").

get_include_dir_path(Application) ->
    get_path_for(Application, "include").

get_include_arg(Application) ->
    arg_for("-include", get_include_dir_path(Application), fun filelib:is_dir/1).

get_config_file_path(Application, CtDir) ->
    get_path_for([Application, CtDir, "app.config"]).

get_config_file_arg(Application, CtDir) ->
    arg_for("-config", get_config_file_path(Application, CtDir), fun filelib:is_regular/1).

arg_for(Switch, Value, Pred) ->
    arg_if(Switch, Value, Pred(Value)).

arg_if(Switch, Value, true) ->
    " " ++ Switch ++ " " ++ Value;
arg_if(_Switch, _Value, _) ->
    "".

get_path_for(Application, File) ->
    filename:absname(filename:join(Application, File)).

get_path_for(Paths) ->
    filename:absname(filename:join(Paths)).
