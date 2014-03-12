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
    topcat_reporter:report_summary(State),
    {reply, ok, State};
handle_call({pre_init_per_suite, SuiteName}, _From, State) ->
    topcat_reporter:report_suite_starts(SuiteName),
    {reply, ok, State};
handle_call({post_init_per_suite, _SuiteName, _Config, _Return}, _From, State) ->
    % Ignore it.
    {reply, ok, State};
handle_call({pre_init_per_testcase, TestcaseName}, _From, State) ->
    topcat_reporter:report_testcase_starts(TestcaseName),
    {reply, ok, State};
handle_call({post_end_per_testcase, TestcaseName, Status}, _From, State) ->
    topcat_reporter:report_testcase_ends(TestcaseName, Status),
    {reply, ok, State};
handle_call({tc_group_results, Results}, _From, State) ->
    %io:format("topcat_server:handle_info(Info=~p, State=~p~n)", [Info, State]),
    NewState = collect_results(Results, State),
    {reply, ok, NewState};
handle_call({on_tc_fail, _TestcaseName, _Status}, _From, State) ->
    % Ignore it.
    {reply, ok, State};
handle_call(Request, _From, State) ->
    io:format("topcat_server:handle_call(Request=~p~n)", [Request]),
    {reply, ok, State}.

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
