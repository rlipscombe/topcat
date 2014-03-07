-module(topcat_cth).
-export([init/2]).

-export([pre_init_per_suite/3, post_end_per_suite/4]).
-export([pre_init_per_testcase/3]).

-include_lib("common_test/include/ct.hrl").

%% @doc Called when the hook is loaded.
init(_Id, _Opts) ->
    State = [],
    {ok, State}.

pre_init_per_suite(SuiteName, InitData, State) ->
    {topcat, 'topcat@localhost'} ! {pre_init_per_suite, SuiteName},
    {InitData, State}.

pre_init_per_testcase(TestcaseName, InitData, State) ->
    {topcat, 'topcat@localhost'} ! {pre_init_per_testcase, TestcaseName},
    io:format("~p~n", [TestcaseName]),
    {InitData, State}.

post_end_per_suite(_SuiteName, Config, _Return, State) ->
    Status = ?config(tc_group_result, Config),
    {topcat, 'topcat@localhost'} ! {tc_group_results, Status},
    {Config, State}.
