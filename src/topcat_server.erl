-module(topcat_server).
-export([start_link/0, stop/0]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, topcat}, ?MODULE, [], []).

stop() ->
    gen_server:call(topcat, stop).

init([]) ->
    State = [],
    {ok, State}.

handle_call(stop, _From, State) ->
    report_summary(State),
    {reply, ok, State};
handle_call({pre_init_per_suite, SuiteName}, _From, State) ->
    report_suite_starts(SuiteName),
    {reply, ok, State};
handle_call({post_init_per_suite, _SuiteName, _Config, _Return}, _From, State) ->
    % Ignore it.
    {reply, ok, State};
handle_call({pre_init_per_testcase, TestcaseName}, _From, State) ->
    report_testcase_starts(TestcaseName),
    {reply, ok, State};
handle_call({post_end_per_testcase, TestcaseName, Status}, _From, State) ->
    report_testcase_ends(TestcaseName, Status),
    {reply, ok, State};
handle_call({tc_group_results, Results}, _From, State) ->
    %io:format("topcat_server:handle_info(Info=~p, State=~p~n)", [Info, State]),
    NewState = collect_results(Results, State),
    {reply, ok, NewState};
handle_call(Request, _From, State) ->
    io:format("topcat_server:handle_call(Request=~p~n)", [Request]),
    {reply, ok, State}.

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

handle_cast(_Request, State) ->
    %io:format("topcat_server:handle_cast(Request=~p, State=~p~n)", [Request, State]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("topcat_server:handle_info(Info=~p, State=~p~n)", [Info, State]),
    {noreply, State}.

% Results is a proplist: [{ok, OK}, {skipped, Skipped}, {failed, Failed}].
% Where OK, Skipped, Failed are [{suite, tc}, ...]
% Not sure what we want, so this'll do for now.
collect_results(Results, State) ->
    [Results | State].

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
