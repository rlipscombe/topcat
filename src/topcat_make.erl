-module(topcat_make).
-export([dir/1]).

%% @todo Get compiler options from somewhere?
dir(Dir) ->
    Sources = [filename:join(Dir, F) || F <- filelib:wildcard("*.erl", Dir)],
    files(Sources).

%% @doc This will return a list of {error, Errors, Warnings} tuples, if there
%% are any errors or warnings.
%% Error is {Filename, [{LineNum,CompilerStage,Message}]}
files(Files) ->
    compile_files(Files, []).

compile_files([], Acc) ->
    lists:reverse(Acc);
compile_files([F|Files], Acc) ->
    Options = [debug_info, warnings_as_errors,
               return_errors, return_warnings],
    Result = compile:file(F, Options),
    compile_files(Files, [Result | Acc]).
