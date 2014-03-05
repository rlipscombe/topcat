-module(topcat).
-export([main/1]).

main(_Args) ->
    %% We load parts of ourself into the new Erlang VM, so we need to do the next bit:
    %%
    % Strip off the escript shebang, and then unpack ourselves.
    ok = filelib:ensure_dir(".topcat/PLACEHOLDER"),

    {ok, EscriptBin} = file:read_file(filename:dirname(code:which(?MODULE))),
    {Pos, _} = binary:match(EscriptBin, <<"PK">>),
    <<_:Pos/binary, ZipBin/binary>> = EscriptBin,
    {ok, _} = zip:extract(ZipBin, [{cwd, ".topcat"}]),
    
    % @todo Don't register self; that results in mixing the receive loop below.
    register(topcat, self()),

    {ok, Node} = slave:start_link(localhost, topcat_child, "-pa .topcat -s topcat_slave -s init stop"),
    monitor_node(Node, true),
    monitor_loop(Node).

monitor_loop(Node) ->
    receive
        {nodedown,Node} ->
            io:format("Slave died; we're done here."),
            ok;
        Other ->
            io:format("~p~n", [Other]),
            monitor_loop(Node)
    end.

    % @todo Consider using the "slave" module instead?
%    Cmd = "erl" ++
%          " -noshell" ++
%          " -pa .topcat" ++
%          " -s topcat_slave",
%    sh(Cmd).
    %ok.

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
