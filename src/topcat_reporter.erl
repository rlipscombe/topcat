-module(topcat_reporter).
-export([report_summary/1,
         report_suite_starts/1, report_testcase_starts/1, report_testcase_ends/2,
         report_make_error/1, report_error/1, report_error/2,
         report_coverage/1]).

report_summary(State) ->
    {OK, Skipped, Failed} = lists:foldl(fun report_suite_summary/2, {0, 0, 0}, State),
    Total = OK + Skipped + Failed,
    Plural = pluralize(Total, "case", "cases"),
    io:format("~p ok, ~p skipped, ~p failed of ~p test ~s\n", [OK, Skipped, Failed, Total, Plural]),
    {OK, Skipped, Failed}.

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
%    io:format("\e[0;92m~p.~p: OK\e[0m\n", [SuiteName, TestcaseName]).

-define(red(X), "\e[0;91m" ++ X ++ "\e[0m").
-define(green(X), "\e[0;92m" ++ X ++ "\e[0m").
-define(yellow(X), "\e[0;93m" ++ X ++ "\e[0m").
-define(cyan(X), "\e[0;96m" ++ X ++ "\e[0m").

report_skipped_test(SuiteName, TestcaseName) ->
    io:format(?yellow("~p.~p: Skipped\n"), [SuiteName, TestcaseName]).

report_failed_test(SuiteName, TestcaseName) ->
    io:format(?red("~p.~p: Failed\n"), [SuiteName, TestcaseName]).

report_suite_starts(SuiteName) ->
    io:format(?cyan("~p...\n"), [SuiteName]).

report_testcase_starts(TestcaseName) ->
    io:format(?cyan("  ~p...\n"), [TestcaseName]).

report_testcase_ends(TestcaseName, ok) ->
    io:format(?cyan("  ~p: ") ?green("~s\n"), [TestcaseName, "OK"]);
report_testcase_ends(TestcaseName, skipped) ->
    io:format(?cyan("  ~p: ") ?yellow("~s\n"), [TestcaseName, "Skipped"]);
report_testcase_ends(TestcaseName, {skipped, {failed, {_SuiteName, SetupName, {Reason, Stacktrace}}}}) ->
    % Skipped because init failed.
    io:format(?cyan("  ~p: ") ?red("~s (~s Failed)\n"), [TestcaseName, "Skipped", SetupName]),
    io:format("  ~p\n", [Reason]),
    report_stacktrace(Stacktrace);
report_testcase_ends(TestcaseName, failed) ->
    io:format(?cyan("  ~p: ") ?red("~s\n"), [TestcaseName, "Failed"]);
report_testcase_ends(TestcaseName, {failed, {Reason, Stacktrace}}) ->
    io:format(?cyan("  ~p: ") ?red("~s\n"), [TestcaseName, "Failed"]),
    io:format("  ~p\n", [Reason]),
    report_stacktrace(Stacktrace);
report_testcase_ends(TestcaseName, Status) ->
    io:format(?cyan("  ~p: ") ?red("~p\n"), [TestcaseName, Status]).

report_stacktrace(Stacktrace) ->
    [report_stackframe(Frame) || Frame <- Stacktrace].

report_stackframe({Module, Function, [], _}) ->
    io:format("  at ~p:~p\n",
              [Module, Function]);
report_stackframe({Module, Function, Arity, Location}) ->
    File = proplists:get_value(file, Location, undefined),
    Line = proplists:get_value(line, Location, 0),
    io:format("  at ~p:~p/~p (~s:~B)\n",
              [Module, Function, Arity, File, Line]).

report_make_error([]) ->
    ok;
report_make_error([{ok, _ModuleName} | Rest]) ->
    report_make_error(Rest);
report_make_error([{ok, _ModuleName, Warnings} | Rest]) ->
    lists:foreach(fun report_compiler_warning/1, Warnings),
    report_make_error(Rest);
report_make_error([{ok, _ModuleName, _Binary, Warnings} | Rest]) ->
    lists:foreach(fun report_compiler_warning/1, Warnings),
    report_make_error(Rest);
report_make_error([{error, Errors, Warnings} | Rest]) ->
    lists:foreach(fun report_compiler_error/1, Errors),
    lists:foreach(fun report_compiler_warning/1, Warnings),
    report_make_error(Rest).

report_compiler_warning(_Warning = {Filename, [{LineNumber, CompilerStage, Message}]}) ->
    io:format(?yellow("~s:~B: ~s\n"), [Filename, line_number(LineNumber), lists:flatten(CompilerStage:format_error(Message))]);
report_compiler_warning(Warning) ->
    io:format(?yellow("~p\n"), [Warning]).

report_compiler_error(_Error = {Filename, [{LineNumber, CompilerStage, Message}]}) ->
    io:format(?red("~s:~B: ~s\n"), [Filename, line_number(LineNumber), lists:flatten(CompilerStage:format_error(Message))]);
report_compiler_error(Error) ->
    io:format(?red("~p\n"), [Error]).

line_number(none) -> 0;
line_number(N) when is_integer(N) -> N;
line_number(_) -> 0.

report_error(Error) ->
    io:format(?red("~p\n"), [Error]).

report_error(Format, Args) ->
    io:format(?red("~s\n"), [lists:flatten(io_lib:format(Format, Args))]).

report_coverage([]) ->
    ok;
report_coverage(Coverage) -> 
    Len = get_max_module_name_len(Coverage),
    Coverage2 = sort_by_pct_covered(Coverage),
    print_coverage(Len, Coverage2).

get_max_module_name_len(Coverage) ->
    lists:max(lists:map(
            fun({Module, {_, _}}) ->
                    length(atom_to_list(Module))
            end, Coverage)).

sort_by_pct_covered(Coverage) ->
    lists:sort(
        fun({_, {CovA, NotCovA}}, {_, {CovB, NotCovB}}) ->
                get_coverage_pct(CovB, NotCovB) =< get_coverage_pct(CovA, NotCovA) 
        end, Coverage).

%sort_by_module_name(Coverage) ->
%    lists:sort(fun({A, _}, {B, _}) -> A =< B end, Coverage).

get_coverage_pct(Cov, NotCov) ->
    trunc((100 * Cov) / (Cov + NotCov)).

print_coverage(Len, Coverage) ->
    lists:foreach(
        fun({Module, {Cov, NotCov}}) ->
                Pct = get_coverage_pct(Cov, NotCov),
                PaddedModule = string:left(atom_to_list(Module), Len, $\s),
                io:format(?cyan("~s : ~B%\n"), [PaddedModule, Pct])
        end, Coverage).
