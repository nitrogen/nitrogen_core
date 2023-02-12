% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (config_handler).
-include("wf.hrl").
-export ([
    get_value/1, get_values/1,
    get_value/2, get_values/2
]).

-callback global() ->   boolean().
-callback init(         handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback finish(       handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback get_value(    Key :: term(),
                        DefaultValue :: term(),
                        handler_config(),
                        handler_state()) -> term().
-callback get_values(   Key :: term(),
                        DefaultValue :: term(),
                        handler_config(),
                        handler_state()) -> [term()].

-optional_callbacks([global/0]).

% Retrieve a configuration value.
-spec get_value(Key :: term()) -> term().
get_value(Key) -> 
    _Value = get_value(Key, undefined).

-spec get_value(Key :: term(), DefaultValue :: term()) -> term().
get_value(Key, DefaultValue) -> 
    _Value = wf_handler:call_readonly(config_handler, get_value, [Key, DefaultValue]).

% get_values(Application, Module, Key, DefaultValue, State) -> Value.
% Retrieve a list of configuration values.
-spec get_values(Key :: term()) -> [term()].
 get_values(Key) -> 
    _Value = get_values(Key, undefined).

-spec get_values(Key :: term(), DefaultValue :: term()) -> [term()].
get_values(Key, DefaultValue) -> 
    _Value = wf_handler:call_readonly(config_handler, get_values, [Key, DefaultValue]).

