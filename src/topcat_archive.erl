-module(topcat_archive).
-export([extract_beams/1]).

extract_beams(TargetDir) ->
    %% We need to inject parts of ourself into the new Erlang VM, so we extract
    %% the beam files somewhere temporary.
    ok = filelib:ensure_dir(TargetDir ++ "/PLACEHOLDER"),

    Path = get_escript_path(),
    {ok, Bin} = file:read_file(Path),
    ZipBin = split_zip(Bin),
    {ok, _Files} = zip:extract(ZipBin, [{cwd, ".topcat"}]).

get_escript_path() ->
    filename:dirname(code:which(?MODULE)).

find_zip(Bin) ->
    {Pos, _} = binary:match(Bin, <<"PK">>),
    Pos.

split_zip(Bin) ->
    Pos = find_zip(Bin),
    <<_:Pos/binary, ZipBin/binary>> = Bin,
    ZipBin.
