-module(topcat_make).
-export([dir/1]).

dir(Dir) ->
    pushd(Dir, fun() -> make() end).

%% @todo ct_make uses compile:file; we could do the same?
%% @todo Get compiler options from somewhere?
make() ->
    %% @todo Options.
    make:all().

pushd(Dir, Fun) ->
    {ok, Cwd} = file:get_cwd(),
    file:set_cwd(Dir),

    Result = Fun(),

    file:set_cwd(Cwd),
    Result.
