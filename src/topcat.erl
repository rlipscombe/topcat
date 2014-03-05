-module(topcat).
-export([main/1]).

main(_Args) ->
    io:format("topcat~n"),
    io:format("~p~n", [filename:dirname(code:which(?MODULE))]),

    extract_beams(),

    % @todo Don't register self; that results in mixing the receive loop below.
    register(topcat, self()),

    % @todo Consider using the "slave" module instead?
    Cmd = "erl" ++
          " -noshell" ++
          " -s topcat_slave",
    sh(Cmd).

%% Adding an escript archive to the code path isn't possible: -pa can't cope
%% with archives with the escript prefix; if it could, we could load ourselves
%% as a module.


%% ...and this only works if we're already _in_ the VM (we're not).
extract_beams() ->
    EscriptBin = file:read_file(filename:dirname(code:which(?MODULE))),
    {Pos, _} = binary:match(EscriptBin, <<"PK">>),
    ZipBin = binary:part(EscriptBin, Pos, byte_size(EscriptBin) - Pos),
    {ok, Beams} = zip:extract(ZipBin, [memory]),
    [erlang:load_module(remove_ext_to_atom(Name), ModuleBin) || {Name, ModuleBin} <- Beams].

sh(Cmd) ->
    Opts = [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    Port = open_port({spawn, Cmd}, Opts),
    io:format("open_port: ~p~n", [Port]),
    sh_loop(Port).

sh_loop(Port) ->
    io:format("sh_loop(~p)~n", [Port]),
    receive
        {Port, {data, {eol, Line}}} ->
            io:format("~s~n", [Line]),
            sh_loop(Port);
        {Port, {data, {noeol, Line}}} ->
            io:format("~s", [Line]),
            sh_loop(Port);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Result}} ->
            {error, Result};
        Other ->
            io:format("~p~n", [Other])
    end.
