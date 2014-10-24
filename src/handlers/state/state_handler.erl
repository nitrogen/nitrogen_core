% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (state_handler).
-include("wf.hrl").
-export ([
    get_state/1, 
    get_state/2,
    set_state/2,
    clear/1,
    clear_all/0
]).


-callback init(         handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback finish(       handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback get_state(    Key :: term(),
                        DefaultValue :: term(), 
                        handler_config(),
                        handler_state()) -> Value :: term().
-callback set_state(    Key :: term(),
                        Value :: term(),
                        handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback clear_all(    handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback clear(        Key :: term(),
                        handler_config(),
                        handler_state()) -> {ok, handler_state()}.

-spec get_state(Key :: term()) -> Value :: term().
%% @doc Retrieve a value from the storage area.
get_state(Key) -> 
    _Value = get_state(Key, undefined).

-spec get_state(Key :: term(), DefaultValue :: term()) -> Value :: term().
%% @doc Retrieve a value from the storage area.
get_state(Key, DefaultValue) ->
    _Value = wf_handler:call_readonly(state_handler, get_state, [Key, DefaultValue]).

-spec set_state(Key :: term(), Value :: term()) -> ok.
%% @doc Put a value into the storage area.
set_state(Key, Value) ->
    ok = wf_handler:call(state_handler, set_state, [Key, Value]).

-spec clear(Key :: term()) -> ok.
%% @doc Remove a value from the storage area.
clear(Key) ->
    ok = wf_handler:call(state_handler, clear, [Key]).

-spec clear_all() -> ok.
%% @doc Clear all values from the storage area.
clear_all() ->
    ok = wf_handler:call(state_handler, clear_all).
