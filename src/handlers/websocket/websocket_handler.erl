% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (websocket_handler).
-include("wf.hrl").
-export ([
    ws_message/2,
    ws_info/2
]).

-callback init(     handler_config(),
                    handler_state()) -> {ok, handler_state()}.
-callback finish(   handler_config(),
                    handler_state()) -> {ok, handler_state()}.
-callback ws_message(websocket_in(),
                    simple_bridge:bridge(),
                    handler_config(),
                    handler_state()) -> {ok, websocket_reply(), handler_state()}.

-callback ws_info(  any(),
                    simple_bridge:bridge(),
                    handler_config(),
                    handler_state()) -> {ok, websocket_reply(), handler_state()}.

-spec ws_message(Msg :: websocket_in(), Bridge :: simple_bridge:bridge()) -> {ok, websocket_reply()}.
ws_message(Msg, Bridge) ->
    {ok, _Reply} = wf_handler:call(websocket_handler, ws_message, [Msg, Bridge]).

-spec ws_info(Msg :: any(), Bridge :: simple_bridge:bridge()) -> {ok, websocket_reply()}.
ws_info(Msg, Bridge) ->
    {ok, _Reply} = wf_handler:call(websocket_handler, ws_info, [Msg, Bridge]).
