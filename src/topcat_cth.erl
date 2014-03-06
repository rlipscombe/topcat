-module(topcat_cth).
-export([init/2]).

-export([post_end_per_suite/4]).

-include_lib("common_test/include/ct.hrl").

-define(COLOR_VERBOSE, "\e[0;36m").
-define(COLOR_OK, "\e[0;32m").
-define(COLOR_SKIPPED, "\e[0;33m").
-define(COLOR_FAILED, "\e[0;31m").
-define(COLOR_RESET, "\e[0m").

-record(state, {old_user :: pid()}).

log_verbose(Format, Data) ->
    io:format(user, ?COLOR_VERBOSE ++ Format ++ ?COLOR_RESET, Data).

log_ok_result(Format, Data) ->
    io:format(user, ?COLOR_OK ++ Format ++ ?COLOR_RESET, Data).
log_skipped_result(Format, Data) ->
    io:format(user, ?COLOR_SKIPPED ++ Format ++ ?COLOR_RESET, Data).
log_failed_result(Format, Data) ->
    io:format(user, ?COLOR_FAILED ++ Format ++ ?COLOR_RESET, Data).

%% @doc Called when the hook is loaded. Once per application, basically.
init(Id, Opts) ->
    log_verbose("init(Id=~p, Opts=~p)~n", [Id, Opts]),
    State = #state{},
    {ok, State}.

post_end_per_suite(SuiteName, Config, Return, State) ->
    Status = ?config(tc_group_result, Config),
    Ok = proplists:get_value(ok, Status, []),
    [log_ok_result("OK: ~p~n", [TestcaseName]) || {_SuiteName, TestcaseName} <- Ok],
    Skipped = proplists:get_value(skipped, Status, []),
    [log_skipped_result("Skipped: ~p~n", [TestcaseName]) || {_SuiteName, TestcaseName} <- Skipped],
    Failed = proplists:get_value(failed, Status, []),
    [log_failed_result("Failed: ~p~n", [TestcaseName]) || {_SuiteName, TestcaseName} <- Failed],
    
    %log_verbose("post_end_per_suite(SuiteName=~p, Config=~p, Return=~p, State=~p~n",
    %          [SuiteName, Config, Return, State]),
    {Config, State}.
