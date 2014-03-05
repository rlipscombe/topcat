-module(topcat_slave).
-export([start/1]).

start([]) ->
    {topcat, 'topcat@roger-p5q'} ! hello,
    ok.

