-module(topcat_server).
-export([start_link/0, stop/0, notify/1]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, topcat).
-define(NODE, 'topcat@localhost').

-record(state, {
        coverage,
        results
        }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Called from the slave.
notify(Event) ->
    ServerRef = {?SERVER, ?NODE},
    gen_server:call(ServerRef, Event).

init([]) ->
    State = #state{coverage = [], results = []},
    {ok, State}.

handle_call(stop, _From, State) ->
    topcat_reporter:report_coverage(State#state.coverage),
    Result = topcat_reporter:report_summary(State#state.results),
    {reply, Result, State};
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
    NewState = collect_results(Results, State),
    {reply, ok, NewState};
handle_call({on_tc_fail, _TestcaseName, _Status}, _From, State) ->
    % Ignore it.
    {reply, ok, State};
handle_call({make_error, Results}, _From, State) ->
    topcat_reporter:report_make_error(Results),
    {reply, ok, State};
handle_call({error, Reason}, _From, State) ->
    topcat_reporter:report_error(Reason),
    {reply, ok, State};
handle_call({coverage, Event}, _From, State) ->
    NewState = collect_coverage(Event, State),
    {reply, ok, NewState};
handle_call({error_logger, Event}, _From, State) ->
    topcat_error_logger:report(Event),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    io:format("topcat_server:handle_call(Request=~p)\n", [Request]),
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("topcat_server:handle_info(Info=~p, State=~p)\n", [Info, State]),
    {noreply, State}.

% Results is a proplist: [{ok, OK}, {skipped, Skipped}, {failed, Failed}].
% Where OK, Skipped, Failed are [{suite, tc}, ...]
% Not sure what we want, so this'll do for now.
collect_results(Results, State = #state{results = Results0}) ->
    State#state{results = [Results | Results0]}.

collect_coverage(Event, State = #state{coverage = Coverage0}) ->
    State#state{coverage = [Event | Coverage0]}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
