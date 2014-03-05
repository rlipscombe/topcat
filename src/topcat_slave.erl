-module(topcat_slave).
-export([start/0]).

start() ->
    {topcat, 'topcat@localhost'} ! hello,
    ok.

