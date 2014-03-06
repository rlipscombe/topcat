-module(topcat_cth).
-export([init/2]).

-export([post_end_per_suite/4]).

-include_lib("common_test/include/ct.hrl").

%% @doc Called when the hook is loaded.
init(_Id, _Opts) ->
    State = [],
    {ok, State}.

post_end_per_suite(_SuiteName, Config, _Return, State) ->
    Status = ?config(tc_group_result, Config),
    {topcat, 'topcat@localhost'} ! {tc_group_results, Status},
    {Config, State}.
