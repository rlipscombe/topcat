-module(topcat_reporter).
-export([report_summary/1, report_suite_starts/1, report_testcase_starts/1, report_testcase_ends/2]).

report_summary(State) ->
    [report_suite_summary(S) || S <- State].

report_suite_summary([]) ->
    ok;
report_suite_summary([{ok, _Tests}|Rest]) ->
    %report_ok_tests(Tests),
    report_suite_summary(Rest);
report_suite_summary([{skipped, Tests}|Rest]) ->
    report_skipped_tests(Tests),
    report_suite_summary(Rest);
report_suite_summary([{failed, Tests}|Rest]) ->
    report_failed_tests(Tests),
    report_suite_summary(Rest).

%report_ok_tests(Tests) ->
%    [report_ok_test(SuiteName, TestcaseName) || {SuiteName, TestcaseName} <- Tests].

report_skipped_tests(Tests) ->
    [report_skipped_test(SuiteName, TestcaseName) || {SuiteName, TestcaseName} <- Tests].

report_failed_tests(Tests) ->
    [report_failed_test(SuiteName, TestcaseName) || {SuiteName, TestcaseName} <- Tests].

%report_ok_test(SuiteName, TestcaseName) ->
%    io:format("\e[0;92m~p.~p...OK\e[0m~n", [SuiteName, TestcaseName]).

report_skipped_test(SuiteName, TestcaseName) ->
    io:format("\e[0;93m~p.~p...Skipped\e[0m~n", [SuiteName, TestcaseName]).

report_failed_test(SuiteName, TestcaseName) ->
    io:format("\e[0;91m~p.~p...Failed\e[0m~n", [SuiteName, TestcaseName]).

report_suite_starts(SuiteName) ->
    io:format("\e[0;96m~p\e[0m~n", [SuiteName]).

report_testcase_starts(TestcaseName) ->
    io:format("  \e[0;96m~p\e[0m~n", [TestcaseName]).

report_testcase_ends(TestcaseName, ok) ->
    io:format("  \e[0;96m~p...\e[0;92m~s\e[0m~n", [TestcaseName, "OK"]);
report_testcase_ends(TestcaseName, skipped) ->
    io:format("  \e[0;96m~p...\e[0;93m~s\e[0m~n", [TestcaseName, "Skipped"]);
report_testcase_ends(TestcaseName, {skipped, {failed, {_SuiteName, SetupName, {Reason, Stacktrace}}}}) ->
    % Skipped because init failed.
    io:format("  \e[0;96m~p...\e[0;91m~s (~s Failed)\e[0m~n", [TestcaseName, "Skipped", SetupName]),
    io:format("  ~p~n", [Reason]),
    report_stacktrace(Stacktrace);
report_testcase_ends(TestcaseName, failed) ->
    io:format("  \e[0;96m~p...\e[0;91m~s\e[0m~n", [TestcaseName, "Failed"]);
report_testcase_ends(TestcaseName, Status) ->
    io:format("  \e[0;96m~p...\e[0;91m~p\e[0m~n", [TestcaseName, Status]).

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
