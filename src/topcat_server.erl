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

handle_call(Request, From, State) ->
    io:format("handle_call(Request=~p, From=~p, State=~p~n)", [Request, From, State]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    io:format("handle_cast(Request=~p, State=~p~n)", [Request, State]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("handle_info(Info=~p, State=~p~n)", [Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
