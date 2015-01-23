-module(topcat_error_logger).
-export([report/1]).
-include("topcat_colored.hrl").

report(Event) ->
    try
        report_event(Event)
    catch
        Cl:Ex ->
            io:format(?red("INTERNAL FAILURE: Exception ~p:~p while reporting event.\n"), [Cl, Ex]),
            error_logger_unknown(Event),
            io:format(?red("STACK TRACE: ~p\n"), [erlang:get_stacktrace()])
    end.

report_event({error, _GL, {_Pid, Fmt, Args}}) ->
    io:format(?yellow(Fmt), Args);
report_event({info_report, _GL, {_Pid, progress, _D}}) ->
    % Ignore progress reports; too many of them absolutely kills performance.
    % They're filtered in the slave, but it doesn't hurt to drop them here as well.
    ignored;
report_event({info_report, _GL, {_Pid, std_info, D}} = Event) ->
    Details = lists:sort(D),
    case Details of
        [{application, App}, {exited, Reason}, {type, _Type}] ->
            io:format(?grey("Application ~w exited with reason ~p\n"), [App, Reason]);
        _ ->
            error_logger_unknown(Event)
    end;
report_event({error_report, _GL, {_Pid, supervisor_report, D}} = Event) ->
    Details = lists:sort(D),
    case Details of
        [{errorContext, Ctx}, {offender, Off}, {reason, Reason}, {supervisor, Sup}] ->
            io:format(
              ?grey("Supervisor ~w had child ~s exit with reason ~s in context ~w\n"),
              [supervisor_name(Sup), format_offender(Off), format_reason(Reason), Ctx]);
        _ ->
            error_logger_unknown(Event)
    end;
report_event({error_report, _GL, {_Pid, crash_report, [Self, Neighbours]}}) ->
    {Class, Reason, Trace} = get_value(error_info, Self),
    Type = format_class(Class),
    ReasonStr = format_reason({Reason, Trace}),
    io:format(
      ?red("Process ~w with ~w neighbours ~s with reason: ~s\n"),
      [process_name(Self), length(Neighbours), Type, ReasonStr]);
report_event(Event) ->
    error_logger_unknown(Event).

format_class(exit) -> "exited";
format_class(_) -> "crashed".

format_reason({bad_return_value, Val}) ->
    ["bad return value ", print_val(Val)];
format_reason({Reason, [{M, F, A, Props}|_]}) when is_atom(M), is_atom(F), is_integer(A), is_list(Props) ->
    [format_reason(Reason), " in ", format_mfa({M, F, A, Props})];
format_reason(Reason) ->
    io_lib:format("~w", [Reason]).

process_name(Process) ->
    case get_value(registered_name, Process, []) of
        [] -> get_value(pid, Process);
        Atom -> Atom
    end.

error_logger_unknown(Event) ->
    io:format(?yellow("~p\n"), [Event]).

print_val(Val) ->
    io_lib:format("~w", [Val]).

%%% @doc The rest of this is borrowed from lager's
%%% error_logger_lager_h, Apache 2.0-licensed.
get_value(Key, Value) ->
    get_value(Key, Value, undefined).
get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {Key, Value} -> Value
    end.

format_mfa({M, F, A}) when is_list(A) ->
    {FmtStr, Args} = format_args(A, [], []),
    io_lib:format("~w:~w("++FmtStr++")", [M, F | Args]);
format_mfa({M, F, A}) when is_integer(A) ->
    io_lib:format("~w:~w/~w", [M, F, A]);
format_mfa({M, F, A, Props}) when is_list(Props) ->
    case get_value(line, Props) of
        undefined ->
            format_mfa({M, F, A});
        Line ->
            [format_mfa({M, F, A}), io_lib:format(" line ~w", [Line])]
    end;
format_mfa([{M, F, A}, _]) ->
    %% this kind of weird stacktrace can be generated by a uncaught throw in a gen_server
    format_mfa({M, F, A});
format_mfa([{M, F, A, Props}, _]) when is_list(Props) ->
    %% this kind of weird stacktrace can be generated by a uncaught throw in a gen_server
    format_mfa({M, F, A, Props});
format_mfa(Other) ->
    io_lib:format("~w", [Other]).

format_args([], FmtAcc, ArgsAcc) ->
    {string:join(lists:reverse(FmtAcc), ", "), lists:reverse(ArgsAcc)};
format_args([H|T], FmtAcc, ArgsAcc) ->
    Str = lists:flatten(io_lib:format("~p", [H])),
    format_args(T, ["~s"|FmtAcc], [Str|ArgsAcc]).

format_offender(Off) ->
    IO = case get_value(mfargs, Off) of
             undefined ->
                 io_lib:format("at module ~w at ~w",
                               [get_value(mod, Off), get_value(pid, Off)]);
             MFArgs ->
                 MFA = format_mfa(MFArgs),
                 Name = get_value(name, Off),
                 io_lib:format("~p started with ~s at ~w",
                               [Name, MFA, get_value(pid, Off)])
         end,
    lists:flatten(IO).

supervisor_name({local, Name}) -> Name;
supervisor_name(Name) -> Name.
