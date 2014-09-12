% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% Copyright (c) 2013-2014 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(nitrogen).
-behaviour(simple_bridge_handler).
-export([
        init_request/2,
        init_request/1,
        handler/2,
        run/0
    ]).

%% Simple Bridge Callout functions
-export([
        run/1,
        ws_init/1,
        ws_message/3,
        ws_info/3,
        ws_terminate/3
    ]).

-deprecated([
        {run,0},
        {init_request,2}
    ]).


%% init_request/2 kept for backwards compatibility, but is no longer needed
init_request(Bridge, _) ->
    init_request(Bridge).

init_request(Bridge) ->
    wf_context:init_context(Bridge).

handler(Module, Config) ->
    wf_handler:set_handler(Module, Config).

run(Bridge) ->
    init_request(Bridge),
    nitrogen_main_handler:run().

ws_init(Bridge) ->
    init_request(Bridge),
    ok.

%ws_message({text, Base64}, Bridge, _State) ->
%    error_logger:info_msg("Text~n"),
%    try {nitrogen_postback, Msg} = binary_to_term(base64:decode(Base64), [safe]),
%        error_logger:info_msg("Received ~p bytes~nDecoded to: ~p",[size(Base64), Msg])
%    catch
%        _:_ -> error_logger:info_msg("Invalid")
%    end,
%    noreply;

ws_message({binary, Bin}, _Bridge, _State) ->
    try binary_to_term(Bin, [safe]) of
        {nitrogen_postback, Msg} ->
            Return = wf_core:run_websocket(Msg),
            {reply, {text, [<<"nitrogen_event:">>,Return]}};
        {page_context, PageContext} ->
            wf_core:init_websocket(PageContext),
            {reply, {text, [
                %% init_websocket has changed the async mode to websocket, so
                %% let's tell the browser about our updated async_mode
                <<"nitrogen_event:">>,
                wf_core:serialize_context(),
                <<"Nitrogen.$enable_websockets();">>
            ]}}
    catch
        Class:Error ->
            error_logger:error_msg("Error in Websocket Message: ~p:~p~n~p~n",
                                   [Class, Error, erlang:get_stacktrace()])
    end.

ws_info({comet_actions, Actions} , _Bridge, _State) ->
    wf:wire(page, page, Actions),
    Return = wf_core:run_websocket_comet(),
    {reply, {text, [<<"nitrogen_system_event:">>, Return]}};
ws_info(Msg, _Bridge, _State) ->
    error_logger:warning_msg("Unhandled message to websocket process: ~p~n",[Msg]),
    noreply.

ws_terminate(_Reason, _Bridge, _State) ->
    close.


%% Deprecated, kept for backwards compatibility. Use nitrogen:run/1 with simple_bridge
run() -> 
    wf_core:run().

