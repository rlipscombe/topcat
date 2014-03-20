-module(topcat_args).
-export([create_slave_args/3]).

create_slave_args(Application, CtDir, CoverEnabled) ->
    LogDir = "logs/ct",
    TestDir = get_path_for(Application, CtDir),

    SlaveArgs = " -pa .topcat" ++
                " -pa " ++ create_code_paths("deps/*/ebin") ++
                " -pa " ++ create_code_paths("apps/*/ebin") ++
                " -s topcat_slave -s init stop" ++
                " -dir " ++ TestDir ++
                " -logdir " ++ get_path_for(Application, LogDir) ++
                get_cover_arg(Application, CoverEnabled) ++
                " -env TEST_DIR " ++ TestDir ++
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
