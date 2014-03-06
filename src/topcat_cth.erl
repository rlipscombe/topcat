-module(topcat_cth).
-export([init/2]).

-export([pre_init_per_suite/3, post_end_per_suite/4]).

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

init(Id, Opts) ->
    log_verbose("init(Id=~p, Opts=~p)~n", [Id, Opts]),
    State = #state{},
    {ok, State}.

%% @doc Called before each suite starts;
%% we'll use it to redirect "user" output.
pre_init_per_suite(SuiteName, InitData, State) ->
    % @todo having redirected the output, how do I now write to the console?
    % @todo How does CT do it (as it did when I broke the hook)?
    % @todo Probably by opening the TTY directly...?
    % @todo Next question: what to do on Jenkins, where we're not on a terminal.
    log_verbose("pre_init_per_suite(SuiteName=~p, InitData=~p, State=~p",
              [SuiteName, InitData, State]),
    NewState = State,
    %NewState = redirect_user_output(State),
    {InitData, NewState}.

redirect_user_output(State) ->
    OldUser = whereis(user),
    unregister(user),
    State#state{old_user=OldUser}.

restore_user_output(State = #state{old_user=OldUser}) ->
    %unregister(user),
    register(user, OldUser),
    State#state{old_user=undefined}.

post_end_per_suite(SuiteName, Config, Return, State) ->
    %NewState = restore_user_output(State),
    Status = ?config(tc_group_result, Config),
    Ok = proplists:get_value(ok, Status, []),
    [log_ok_result("OK: ~p~n", [TestcaseName]) || {_SuiteName, TestcaseName} <- Ok],
    Skipped = proplists:get_value(skipped, Status, []),
    [log_skipped_result("Skipped: ~p~n", [TestcaseName]) || {_SuiteName, TestcaseName} <- Skipped],
    Failed = proplists:get_value(failed, Status, []),
    [log_failed_result("Failed: ~p~n", [TestcaseName]) || {_SuiteName, TestcaseName} <- Failed],

    NewState = State,
    %io:format(user, "\e[0;35mpost_end_per_suite(SuiteName=~p, Config=~p, Return=~p, State=~p~n",
    %          [SuiteName, Config, Return, State]),
    {Config, NewState}.
