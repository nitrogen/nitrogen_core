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

call_main_handler_ws_init() ->
    case erlang:function_exported(nitrogen_main_handler, ws_init, 0) of
        true -> nitrogen_main_handler:ws_init();
        false -> ok
    end.

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
    call_main_handler_ws_init(),
    ok.

ws_message({text, _Other}, _Bridge, _State) ->
    noreply;
ws_message({binary, Bin}, _Bridge, _State) ->
    Reply = try
        ws_message_catched(binary_to_term(Bin, [safe]))
    catch
        Class:Error ->
            Stacktrace = erlang:get_stacktrace(),
            CrashReturn = wf_core:run_websocket_crash(Class, Error, Stacktrace),
            [<<"nitrogen_event:">>,CrashReturn]
    end,
    {reply, {text, Reply}}.

ws_message_catched({nitrogen_postback,Msg}) ->
    Return = wf_core:run_websocket(Msg),
    [<<"nitrogen_event:">>,Return];
ws_message_catched(flush_switchover_comet_actions) ->
    %% If there are any actions that weren't 
    ReconnectionRecovery = <<"Nitrogen.$reconnect_system();">>,
    case action_comet:get_actions_and_register_new_websocket_pid(self()) of
        [] -> 
            [<<"nitrogen_system_event:">>, ReconnectionRecovery];
        Actions ->
            wf:wire(page, page, Actions),
            Return = wf_core:run_websocket_comet(),
            [<<"nitrogen_system_event:">>, [ReconnectionRecovery, Return]]
    end;
ws_message_catched({page_context, PageContext}) ->
    wf_core:init_websocket(PageContext),
    [
        %% init_websocket has changed the async mode to websocket, so
        %% let's tell the browser about our updated async_mode
        <<"nitrogen_event:">>,
        wf_core:serialize_context(),
        <<"Nitrogen.$enable_websockets();">>
    ].

ws_info({comet_actions, Actions} , _Bridge, _State) ->
    wf:wire(page, page, Actions),
    Return = wf_core:run_websocket_comet(),
    {reply, {text, [<<"nitrogen_system_event:">>, Return]}};
ws_info(Msg, _Bridge, _State) ->
    error_logger:warning_msg("Unhandled message(~p) to websocket process (~p)~n", [Msg, self()]),
    noreply.

ws_terminate(_Reason, _Bridge, _State) ->
    ok.


%% Deprecated, kept for backwards compatibility. Use nitrogen:run/1 with simple_bridge
run() -> 
    wf_core:run().

