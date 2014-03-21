-module(topcat_args).
-export([parse_args/1, create_slave_args/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

%% @doc Convert string args into something like that returned by
%% init:get_arguments/0.
%% Note that we can't use init:get_arguments from an escript.
%% Also note that, in escript, this is already tokenised.
parse_args(Argv) ->
    parse_args(Argv, {}, []).

parse_args([], SwAcc, Acc) ->
    lists:reverse(add_switch(SwAcc, Acc));
parse_args([A|Args], SwAcc, Acc) when hd(A) == $- ->
    Key = switch_to_atom(A),
    parse_args(Args, {Key, []}, add_switch(SwAcc, Acc));
parse_args([A|Args], {Key, Vals}, Acc) ->
    parse_args(Args, {Key, [A|Vals]}, Acc).

switch_to_atom(S) ->
    list_to_atom(tl(S)).

add_switch({}, Acc) ->
    Acc;
add_switch({Key, Vals}, Acc) ->
    [{Key, lists:reverse(Vals)} | Acc].

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

-ifdef(TEST).
-define(test(Expected, String),
        ?_assertMatch(Expected, parse_args(string:tokens(String, " ")))).
parse_args_test_() ->
    [
        {"no args results in empty list",
         ?test([], "")},

        % unadorned args are ignored -- you'd use init:get_plain_arguments/0
        % for those.
        {"single-arg switch",
         ?test([{foo, ["bar"]}], "-foo bar")},

        {"multi-arg switch",
         ?test([{foo, ["bar", "baz"]}], "-foo bar baz")},

        {"multiple single-arg switches",
         ?test([{foo, ["bar"]}, {foo, ["baz"]}], "-foo bar -foo baz")},

        {"switch with no args",
         ?test([{quux, []}], "-quux")},

        {"mixture 1",
         ?test(
                [{a, []}, {b, ["B"]}, {c, ["C", "D"]}, {q, ["P"]}],
                "-a -b B -c C D -q P")},

        {"single-arg followed by no-arg",
         ?test(
                [{q, ["Q"]}, {z, []}],
                "-q Q -z")},

        {"mixture 2",
         ?test(
                [{a, []}, {b, ["B"]}, {c, ["C", "D"]}, {q, ["P"]}, {e, ["F"]}, {e, ["G"]}, {q, ["Q"]}, {z, []}],
                "-a -b B -c C D -q P -e F -e G -q Q -z")}
        ].
-endif.
