-module(topcat_args).
-export([create_slave_args/4]).

create_slave_args(Application, CtDir, CoverEnabled, CtExtraParams) ->
    TestDir = get_path_for(Application, CtDir),

    SlaveArgs = " -pa .topcat" ++
                " -pa " ++ create_code_paths("deps/*/ebin") ++
                " -pa " ++ create_code_paths("apps/*/ebin") ++
                " -s topcat_slave -s init stop" ++
                " -dir " ++ TestDir ++
                get_cover_arg(Application, CoverEnabled) ++
                " -env TEST_DIR " ++ TestDir ++
                get_include_arg(Application) ++
                get_config_file_arg(Application, CtDir) ++
                " " ++ fix_ct_extra_params(Application, CtExtraParams),
    SlaveArgs.

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

fix_ct_extra_params(BaseDir, Extra) ->
    Params = parse_ct_extra_params(Extra),
    combine_ct_extra(fix_ct_extra(BaseDir, Params, [])).

parse_ct_extra_params(Extra) ->
    Params = string:tokens(Extra, " "),
    parse_ct_extra(Params, [], []).

combine_ct_extra(Params) ->
    CombineValues = fun(X, Acc) ->
            Acc ++ " " ++ X
    end,
    CombineSwitches = fun({Switch, Values}, Acc) ->
            Acc ++ " -" ++ Switch ++
            lists:foldl(CombineValues, " ", Values)
    end,
    lists:foldl(CombineSwitches, "", Params).

parse_ct_extra([], SwAcc, Acc) ->
    lists:reverse([SwAcc|Acc]);
parse_ct_extra([P | Params], SwAcc, Acc) when hd(P) == $- ->
    parse_ct_extra(Params, {string:substr(P, 2), []}, [SwAcc|Acc]);
parse_ct_extra([P | Params], {Sw, Vs} = _SwAcc, Acc) ->
    parse_ct_extra(Params, {Sw, [P|Vs]}, Acc).

fix_ct_extra(_Base, [], Acc) ->
    lists:reverse(Acc);
fix_ct_extra(Base, [[]|Rest], Acc) ->
    fix_ct_extra(Base, Rest, Acc);
fix_ct_extra(Base, [{"pa", Paths}|Rest], Acc) ->
    fix_ct_extra(Base, Rest, [{"pa", fix_paths(Base, Paths)} | Acc]);
fix_ct_extra(Base, [{"logdir", LogDirs}|Rest], Acc) ->
    fix_ct_extra(Base, Rest, [{"logdir", fix_paths(Base, LogDirs)} | Acc]);
fix_ct_extra(Base, [X|Rest], Acc) ->
    fix_ct_extra(Base, Rest, [X|Acc]).

fix_paths(Base, Paths) ->
    lists:map(fun(P) -> fix_path(Base, P) end, Paths).

fix_path(Base, Path) ->
    get_path_for(Base, Path).
