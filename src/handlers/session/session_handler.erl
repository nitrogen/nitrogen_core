% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

%
% The session_handler provides a place to store values on the server
% between requests.
%
% An application can define a custom session handler to control
% how Nitrogen manages session values.

-module (session_handler).
-include("wf.hrl").
-export ([
    get_value/1, 
    get_value/2, 
    set_value/2, 
    clear_all/0,
    session_id/0
]).

-callback init(         handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback finish(       handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback get_value(    Key :: term(),
                        DefaultValue :: term(),
                        handler_config(),
                        handler_state()) -> {ok, Value :: term(), handler_state()}.
-callback set_value(    Key :: term(),
                        Value :: term(),
                        handler_config(),
                        handler_state()) -> {ok, OldValue :: term(), handler_state()}.
-callback clear_all(    handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback session_id(   handler_config(),
                        handler_state()) -> {ok, Sessionid:: term(), handler_state()}.


% get(Key, DefaultValue, State, Key, DefaultValue) -> {ok, Value, NewState}.
% Retrieve a value from the storage area.
get_value(Key) ->
    _Value = get_value(Key, undefined).

% get(Key, DefaultValue, State, Key, DefaultValue) -> {ok, Value, NewState}.
% Retrieve a value from the storage area.
get_value(Key, DefaultValue) ->
    {ok, Value} = wf_handler:call(session_handler, get_value, [Key, DefaultValue]),
    Value.

% set_value(Key, Value, State) -> {ok, NewState}.
% Put a value into the storage area.
set_value(Key, Value) ->
    {ok, OldValue} = wf_handler:call(session_handler, set_value, [Key, Value]),
    OldValue.

% clear_all(State) -> {ok, NewState}.
% Clear all values from the storage area.
clear_all() ->
    ok = wf_handler:call(session_handler, clear_all).

% session_id() -> SessionId
% Return the unique session id
session_id() ->
    {ok, SessionId} = wf_handler:call(session_handler, session_id),
    SessionId.
