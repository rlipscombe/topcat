-module(topcat_error_logger).
-export([report/1]).
-include("topcat_colored.hrl").

report({error, _, {_Pid, Fmt, Args}}) ->
    io:format(?yellow(Fmt), Args);
report({info_report, _, {_Pid, progress, _D}}) ->
    % Ignore progress reports; too many of them absolutely kills performance.
    % They're actually filtered in the slave.
    ignored;
report({info_report, _, {_Pid, std_info, D}} = Event) ->
    Details = lists:sort(D),
    case Details of
        [{application, App}, {exited, Reason}, {type, _Type}] ->
            io:format(?grey("Application ~w exited with reason ~p\n"), [App, Reason]);
        _ ->
            error_logger_unknown(Event)
    end;
report(Event) ->
    error_logger_unknown(Event).
    
error_logger_unknown(Event) ->
    io:format(?yellow("~p\n"), [Event]).
