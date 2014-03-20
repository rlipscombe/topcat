-module(topcat_make).
-export([dir/1]).

%% @todo Get compiler options from somewhere?
dir(Dir) ->
    Opts = [debug_info, warnings_as_errors, {outdir, Dir}] ++ get_include_opts(),
    Sources = [filename:join(Dir, F) || F <- filelib:wildcard("*.erl", Dir)],
    files(Sources, Opts).

get_include_opts() ->
    case init:get_argument(include) of
        {ok, Includes} ->
            lists:map(fun(X) -> {i, X} end, lists:concat(Includes));
        _ ->
            []
    end.

%% @doc This will return a list of {error, Errors, Warnings} tuples, if there
%% are any errors or warnings.
%% Error is {Filename, [{LineNum,CompilerStage,Message}]}
files(Files, Opts) ->
    compile_files(Files, Opts, []).

compile_files([], _Opts, Acc) ->
    lists:reverse(Acc);
compile_files([F|Files], Opts, Acc) ->
    Options = Opts ++ [return_errors, return_warnings],
    Result = compile:file(F, Options),
    compile_files(Files, Opts, [Result | Acc]).
