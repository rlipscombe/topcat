-module(topcat_cover).
-export([tidy_spec/2]).

%% @doc Ensure that the paths in the cover.spec file are fully-qualified.
tidy_spec(Path, NewPath) ->
    {ok, Specs} = file:consult(Path),
    NewSpecs = tidy_specs(Specs),
    io:format("NewSpecs ~p\n", [NewSpecs]),
    write_specs(NewPath, NewSpecs).

tidy_specs(Specs) ->
    tidy_specs(Specs, []).

tidy_specs([], Acc) ->
    lists:reverse(Acc);
tidy_specs([{export, Path} | Rest], Acc) ->
    tidy_specs(Rest, [{export, filename:absname(Path)} | Acc]);
tidy_specs([Value | Rest], Acc) ->
    tidy_specs(Rest, [Value | Acc]).

write_specs(Path, Specs) ->
    ok = file:write_file(Path, format_specs(Specs, [])).

format_specs([], Acc) ->
    lists:reverse(Acc);
format_specs([Term|Rest], Acc) ->
    format_specs(Rest, [io_lib:format("~p.\n", [Term]) | Acc]).
