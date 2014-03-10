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
handle_call(_Request, _From, State) ->
    %io:format("topcat_server:handle_call(Request=~p, From=~p, State=~p~n)", [Request, From, State]),
    {reply, ok, State}.

report_summary(State) ->
    [report_suite_summary(S) || S <- State].

report_suite_summary([]) ->
    ok;
report_suite_summary([{ok, Tests}|Rest]) ->
    %report_ok_tests(Tests),
    report_suite_summary(Rest);
report_suite_summary([{skipped, Tests}|Rest]) ->
    report_skipped_tests(Tests),
    report_suite_summary(Rest);
report_suite_summary([{failed, Tests}|Rest]) ->
    report_failed_tests(Tests),
    report_suite_summary(Rest).

report_ok_tests(Tests) ->
    [io:format("\e[0;92m~p.~p...OK\e[0m~n", [SuiteName, TestcaseName]) || {SuiteName, TestcaseName} <- Tests].

report_skipped_tests(Tests) ->
    [io:format("\e[0;93m~p.~p...Skipped\e[0m~n", [SuiteName, TestcaseName]) || {SuiteName, TestcaseName} <- Tests].

report_failed_tests(Tests) ->
    [io:format("\e[0;91m~p.~p...Failed\e[0m~n", [SuiteName, TestcaseName]) || {SuiteName, TestcaseName} <- Tests].

handle_cast(_Request, State) ->
    %io:format("topcat_server:handle_cast(Request=~p, State=~p~n)", [Request, State]),
    {noreply, State}.

handle_info(_Info = {tc_group_results, Results}, State) ->
    %io:format("topcat_server:handle_info(Info=~p, State=~p~n)", [Info, State]),
    NewState = collect_results(Results, State),
    {noreply, NewState};
handle_info({pre_init_per_suite, SuiteName}, State) ->
    io:format("\e[0;96m~p\e[0m~n", [SuiteName]),
    {noreply, State};
handle_info({pre_init_per_testcase, SuiteName}, State) ->
    io:format("  \e[0;96m~p\e[0m~n", [SuiteName]),
    {noreply, State};
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
