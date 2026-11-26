% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_config_handler).
-include("wf.hrl").
-behaviour (config_handler).
-export ([
    init/2,
    finish/2,
    get_value/4,
    get_values/4
]).

init(_Config, _State) ->
    {ok, []}.

finish(_Config, _State) ->
    {ok, []}.

get_value(Key, DefaultValue, Config, State) ->
    case get_values(Key, [DefaultValue], Config, State) of
        [Value] ->
            Value;
        Values ->
            error_logger:error_msg("Too many matching config values for key: ~p~n", [Key]),
            throw({nitrogen_error, too_many_matching_values, Key, Values})
    end.

get_values(Key, DefaultValue, Config, State) ->
    %% By default, use nitrogen_core as the app (for Nitrogen 2.4+), however,
    %% for backwards compatibility, also check for the nitrogen app.
    DefaultApps = [nitrogen_core, nitrogen],
    Apps = case application:get_env(nitrogen_core, application_config_key, []) of
               App when is_list(App) ->
                   App ++ DefaultApps;
               App ->
                   [App] ++ DefaultApps
           end,
    %% If a nitrogen_core configuration key application_config_fn
    %% exists and is a function of arity 2, dispatch to that function.
    %%
    %% config_fn(Key, DefaultValue) -> {true, Value} | any().
    case configuration_function(Key, DefaultValue) of
        {ok, Value} ->
            [Value];
        _ ->
            get_values(Apps, Key, DefaultValue, Config, State)
    end.

get_values([], _Key, DefaultValue, _Config, _State) ->
    DefaultValue;
get_values([App|Apps], Key, DefaultValue, _Config, _State) ->
    case application:get_env(App, Key) of
        {ok, Value} ->
            [Value];
        undefined ->
            get_values(Apps, Key, DefaultValue, _Config, _State)
    end.

configuration_function(Key, DefaultValue) ->
    configuration_function(configuration_function_defined(), Key, DefaultValue).

configuration_function({false, undefined}, _Key, _DefaultValue) ->
    false;
configuration_function({false, Fn}, _Key, _DefaultValue) ->
    Msg = "application_config_fn (~p) specified but not a function or not arity 2",
    error_logger:error_msg(Msg, [Fn]),
    throw({nitrogen_error, invalid_configuration_function, Fn});
configuration_function({true, Fn}, Key, DefaultValue) ->
    Fn(Key, DefaultValue).

configuration_function_defined() ->
    case application:get_env(nitrogen_core, application_config_fn, []) of
        [] ->
            {false, undefined};
        Fn ->
            {is_function(Fn, 2), Fn}
    end.
