-module(topcat_cth).
-export([init/2]).

-export([pre_init_per_suite/3, post_init_per_suite/4]).
-export([post_end_per_suite/4]).

-export([pre_init_per_testcase/3]).
-export([on_tc_fail/3, on_tc_skip/3]).
-export([post_end_per_testcase/4]).

-include_lib("common_test/include/ct.hrl").

%% @doc Called when the hook is loaded.
init(_Id, _Opts) ->
    State = [],
    {ok, State}.

pre_init_per_suite(SuiteName, InitData, State) ->
    notify({pre_init_per_suite, SuiteName}),
    {InitData, State}.

post_init_per_suite(SuiteName, Config, Return, State) ->
    notify({post_init_per_suite, SuiteName, Config, Return}),
    {Config, State}.

on_tc_fail(TestcaseName, Reason, State) ->
    notify({on_tc_fail, TestcaseName, Reason}),
    State.

on_tc_skip(TestcaseName, Reason, State) ->
    notify({on_tc_skip, TestcaseName, Reason}),
    State.

pre_init_per_testcase(TestcaseName, InitData, State) ->
    notify({pre_init_per_testcase, TestcaseName}),
    {InitData, State}.

post_end_per_testcase(TestcaseName, Config, _Return, State) ->
    Status = ?config(tc_status, Config),
    notify({post_end_per_testcase, TestcaseName, Status}),
    NewReturn = case Status of
        {skipped, {failed, _} = Reason} -> {fail, {skipped, Reason}};
        _ -> Config
    end,
    {NewReturn, State}.

post_end_per_suite(_SuiteName, Config, _Return, State) ->
    Status = ?config(tc_group_result, Config),
    notify({tc_group_results, Status}),
    {Config, State}.

notify(Request) ->
    ServerRef = {topcat, 'topcat@localhost'},
    gen_server:call(ServerRef, Request).
