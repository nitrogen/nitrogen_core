% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2023 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(default_websocket_handler).
-behaviour(websocket_handler).
-handler_level(application).
-include("wf.hrl").
-export ([
    init/2, 
    finish/2,
    ws_message/4,
    ws_info/4
]).

-spec init(handler_config(), handler_state()) -> {ok, handler_state()}.
init(_Config, State) -> 
    {ok, State}.

-spec finish(handler_config(), handler_state()) -> {ok, handler_state()}.
finish(_Config, State) -> 
    {ok, State}.

%% Messages from the client
-spec ws_message(
        Msg :: websocket_in(),
        Bridge :: simple_bridge:bridge(),
        handler_config(),
        handler_state()) -> {ok, websocket_reply(), handler_state()}.
ws_message(Msg, Bridge, _Config, State) ->
	Reply = try
        %% Relying on the previously established context for this current
        %% websocket connection, send the message to the page to be handled.
        dispatch_to_page_module(ws_message_event, Msg, Bridge)
    catch
        _:_:_ ->
            %% If the above dispatch fails, that just means the message is
            %% being sent without an established context. That's alright, we'll
            %% just handle it at the bottom of this module.
			try handle_ws_message(Msg, Bridge)
			catch T:E:S ->
                %% We're not currently in a request, so let's just directly use
                %% logger rather then wf:error
				logger:error("Error processing websocket message from client.~n"
                             "Message: ~p~n"
                             "Error~p:~p~n~p",[Msg,T,E,S]),
				noreply
			end
     end,
    {ok, Reply, State}.

%% Erlang messages to the websocket process's mailbox.  The function is
%% structured very similarly to ws_message/4, so the comments have been
%% removed.  Admittedly, it could brefactored, but this is simple enough we
%% won't worry about it.
-spec ws_info(
        Msg :: any(),
        Bridge :: simple_bridge:bridge(),
        handler_config(),
        handler_state()) -> {ok, websocket_reply(), handler_state()}.
ws_info(Msg, Bridge, _Config, State) ->
	Reply = try
        dispatch_to_page_module(ws_info_event, Msg, Bridge)
    catch
        _:_:_ ->
			try handle_ws_info(Msg, Bridge)
			catch T:E:S ->
				logger:error("Error processing message to websocket process.~n"
                             "Message: ~p~n"
                             "Error~p:~p~n~p",[Msg,T,E,S]),
				noreply
			end
     end,
    {ok, Reply, State}.

dispatch_to_page_module(EventFun, Msg, Bridge) ->
    Mod = wf:page_module(),
    case erlang:function_exported(Mod, EventFun, 2) of
        true ->
            try Mod:EventFun(Msg, Bridge)
            catch T:E:S ->
                wf:error("There was an error processing ~p:ws_message_event/1.~n"
						 "Message: ~p~nError: ~p:~p~nStack Trace: ~p",
						 [Msg, T, E, S]),
                noreply
            end;
        false ->
			wf:info("~p:~p/1 is not exported.~p~nMessage received: ~p.", [Mod, EventFun, Msg]),
			handle_ws_message(Msg, Bridge)
     end.


%% NOTE: These next two functions are provided as stubs in case you decide to
%% copy this nitrogen websocket handler.

%% Handle a message from the client
-spec handle_ws_message(websocket_in(), Bridge :: simple_bridge:bridge()) -> websocket_reply().
handle_ws_message({text, _Text}, _Bridge) ->
    %% Handle a text-encoded websocket message from the client
	noreply;
handle_ws_message({binary, _Binary}, _Bridge) ->
    %% Handle a binary-encoded websocket message from the client
	noreply;
handle_ws_message({decoded, _Decoded}, _Bridge) ->
    %% Handle a term that was BERT-encoded from the client. The term is
    %% auto-decoded.
	noreply.

%% Handle an Erlang message from within the system to the websocket process's
%% mailbox.
-spec handle_ws_info(Msg :: any(), Bridge :: simple_bridge:bridge()) -> websocket_reply().
handle_ws_info(_Msg, _Bridge) ->
    noreply.
