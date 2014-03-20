-module(topcat_config).
-export([get_config/1, get_ct_dir/1, get_cover_enabled/1]).

-define(DEFAULT_CT_DIR, "tests").
-define(DEFAULT_COVER_ENABLED, false).

-record(config, { local, global }).

%% @doc Get the effective rebar configuration by combining app/foo/rebar.config
%% and ./rebar.config
get_config(Application) ->
    LocalConfigFile = filename:join(Application, "rebar.config"),
    GlobalConfigFile = "rebar.config",
    #config{
        local = get_config_result(file:consult(LocalConfigFile)),
        global = get_config_result(file:consult(GlobalConfigFile))}.

get_ct_dir(Config) ->
    get_config_value(ct_dir, Config, ?DEFAULT_CT_DIR).

get_cover_enabled(Config) ->
    get_config_value(cover_enabled, Config, ?DEFAULT_COVER_ENABLED).

get_config_result({ok, Config}) ->
    Config;
get_config_result(_) ->
    [].

%% @doc Look in the local config for the setting; then look in the global
%% config.
get_config_value(Key, Config, Default) ->
    proplists:get_value(
        Key, Config#config.local,
        proplists:get_value(Key, Config#config.global,
                            Default)).
