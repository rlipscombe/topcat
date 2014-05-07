-module(topcat_port).
-export([run/2]).

-record(state, {
        cover_analysing = false     % ignore verbose cover analysis output.
        }).

%% @doc Returns ok | {exit_status, S}.
run(Cmd, Opts) ->
    Port = erlang:open_port({spawn, Cmd}, Opts),
    loop(Port, #state{}).

loop(Port, State) ->
    receive
        % We can occasionally receive 'exit_status' before the final 'data'.
        % Since we're only printing the output, we can safely ignore the
        % originator.
        {_Port, {data, Data}} ->
            loop(Port, data(Data, State));
        % 'exit_status' is sent when the process exits (but sometimes before
        % the final 'data').
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Status}} ->
            {exit_status, Status};
        % Otherwise, just print it out.
        Other ->
            io:format("~p\n", [Other]),
            loop(Port, State)
    end.

data({eol, "Cover analysing..."}, State) ->
    % We're entering the cover analysis phase; ignore verbose output from the
    % cover module.
    State#state{cover_analysing = true};
data({eol, _Line = "Analysis includes data from imported files"}, State = #state{cover_analysing = true}) ->
    State;
data({eol, Line}, State = #state{cover_analysing = true}) ->
    RE = "\\[\".*\\.coverdata\"\\]",
    case re:run(Line, RE) of
        {match, _} ->
            ignore;
        _ ->
            io:format("~s\n", [Line])
    end,
    State;
data({eol, Line}, State) ->
    io:format("~s\n", [Line]),
    State;
data({noeol, Line}, State) ->
    io:format("~s", [Line]),
    State.
