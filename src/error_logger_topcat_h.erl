-module(error_logger_topcat_h).
-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2, code_change/3]).

init([]) ->
    {ok, undefined}.

handle_call(_Req, State) ->
    {ok, ok, State}.

handle_event({info_report, _, {_Pid, progress, _D}}, State) ->
    % Don't even bother forwarding these.
    {ok, State};
handle_event(Event, State) ->
    topcat_server:notify({error_logger, Event}),
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
