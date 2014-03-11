-module(topcat_reporter).
-export([report_summary/1, report_suite_starts/1, report_testcase_starts/1, report_testcase_ends/2]).

report_summary(State) ->
    {OK, Skipped, Failed} = lists:foldl(fun report_suite_summary/2, {0, 0, 0}, State),
    Total = OK + Skipped + Failed,
    Plural = pluralize(Total, "case", "cases"),
    io:format("~p ok, ~p skipped, ~p failed of ~p test ~s~n", [OK, Skipped, Failed, Total, Plural]).

pluralize(1, Singular, _) -> Singular;
pluralize(_, _, Plural) -> Plural.

report_suite_summary([], Acc) ->
    Acc;
report_suite_summary([{Result, Tests}|Rest], Acc) ->
    Acc1 = report_tests(Result, Tests, Acc),
    report_suite_summary(Rest, Acc1).

report_tests(ok, Tests, {OK, Skipped, Failed}) ->
    %[report_ok_test(SuiteName, TestcaseName) || {SuiteName, TestcaseName} <- Tests],
    N = length(Tests),
    {OK + N, Skipped, Failed};
report_tests(skipped, Tests, {OK, Skipped, Failed}) ->
    [report_skipped_test(SuiteName, TestcaseName) || {SuiteName, TestcaseName} <- Tests],
    N = length(Tests),
    {OK, Skipped + N, Failed};
report_tests(failed, Tests, {OK, Skipped, Failed}) ->
    [report_failed_test(SuiteName, TestcaseName) || {SuiteName, TestcaseName} <- Tests],
    N = length(Tests),
    {OK, Skipped, Failed + N}.

%report_ok_test(SuiteName, TestcaseName) ->
%    io:format("\e[0;92m~p.~p: OK\e[0m~n", [SuiteName, TestcaseName]).

report_skipped_test(SuiteName, TestcaseName) ->
    io:format("\e[0;93m~p.~p: Skipped\e[0m~n", [SuiteName, TestcaseName]).

report_failed_test(SuiteName, TestcaseName) ->
    io:format("\e[0;91m~p.~p: Failed\e[0m~n", [SuiteName, TestcaseName]).

report_suite_starts(SuiteName) ->
    io:format("\e[0;96m~p...\e[0m~n", [SuiteName]).

report_testcase_starts(TestcaseName) ->
    io:format("  \e[0;96m~p...\e[0m~n", [TestcaseName]).

report_testcase_ends(TestcaseName, ok) ->
    io:format("  \e[0;96m~p: \e[0;92m~s\e[0m~n", [TestcaseName, "OK"]);
report_testcase_ends(TestcaseName, skipped) ->
    io:format("  \e[0;96m~p: \e[0;93m~s\e[0m~n", [TestcaseName, "Skipped"]);
report_testcase_ends(TestcaseName, {skipped, {failed, {_SuiteName, SetupName, {Reason, Stacktrace}}}}) ->
    % Skipped because init failed.
    io:format("  \e[0;96m~p: \e[0;91m~s (~s Failed)\e[0m~n", [TestcaseName, "Skipped", SetupName]),
    io:format("  ~p~n", [Reason]),
    report_stacktrace(Stacktrace);
report_testcase_ends(TestcaseName, failed) ->
    io:format("  \e[0;96m~p: \e[0;91m~s\e[0m~n", [TestcaseName, "Failed"]);
report_testcase_ends(TestcaseName, Status) ->
    io:format("  \e[0;96m~p: \e[0;91m~p\e[0m~n", [TestcaseName, Status]).

report_stacktrace(Stacktrace) ->
    [report_stackframe(Frame) || Frame <- Stacktrace].

report_stackframe({Module, Function, [], _}) ->
    io:format("  at ~p:~p~n",
              [Module, Function]);
report_stackframe({Module, Function, Arity, Location}) ->
    File = proplists:get_value(file, Location, undefined),
    Line = proplists:get_value(line, Location, 0),
    io:format("  at ~p:~p/~p (~s:~p)~n",
              [Module, Function, Arity, File, Line]).
