-module(topcat_args).
-export([parse_args/1, get_all_arguments/2]).

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

%% @doc Search Args for Key, collecting the values together into a single list.
get_all_arguments(Key, Args) ->
    lists:append(proplists:get_all_values(Key, Args)).

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

get_all_arguments_test_() ->
    [
        ?_assertMatch([], get_all_arguments(key, [])),
        ?_assertMatch([], get_all_arguments(other, [{key, ["value"]}])),
        ?_assertMatch(["bar"], get_all_arguments(foo, [{foo, ["bar"]}])),
        ?_assertMatch(["bar", "baz"], get_all_arguments(foo, [{foo, ["bar"]}, {foo, ["baz"]}])),
        ?_assertMatch(["bar", "baz"], get_all_arguments(foo, [{foo, ["bar", "baz"]}])),

        ?_assertMatch(["bar", "baz", "quux"],
                      get_all_arguments(foo, [{foo, ["bar"]}, {key, "value"}, {foo, ["baz", "quux"]}]))
        ].
-endif.
