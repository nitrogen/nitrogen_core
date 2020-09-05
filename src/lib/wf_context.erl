% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_context).
-include("wf.hrl").

-export([
        bridge/0,
        bridge/1,

        in_request/0,
        socket/0,

        path/0,
        protocol/0,
        uri/0,
        url/0,

        peer_ip/0,
        peer_ip/1,
        peer_ip/2,

        request_method/0,
        request_body/0,
        status_code/0,
        status_code/1,

        content_type/1,
        content_type/0,
        encoding/0,
        encoding/1,
        download_as/1,
        headers/0,
        header/1,
        header/2,

        anchor/1,
        anchor/0,

        data/0,
        data/1,
        clear_data/0,

        add_action/2,
        actions/0,
        next_action/0,
        action_queue/0,
        action_queue/1,
        clear_action_queue/0,
        new_action_queue/0,

        page_context/0,
        page_context/1,
       
        entry_point/0,
        entry_point/1,

        series_id/0,
        series_id/1,

        page_module/0,
        page_module/1,

        path_info/0,
        path_info/1,

        async_mode/0,
        async_mode/1,

        event_context/0,
        event_context/1,

        type/0,
        type/1,

        event_module/0,
        event_module/1,

        event_tag/0,
        event_tag/1,

        event_validation_group/0,
        event_validation_group/1,
        event_handle_invalid/0,
        event_handle_invalid/1,

        handlers/0,
        handlers/1,
        handler/1,
        restore_handler/1,

        init_context/1,
        make_handler/2,

        context/0,
        context/1
    ]).

-export([increment/1]).

%% Exports for backwards compatibility
-export([
        request_bridge/0,
        request_bridge/1,
        response_bridge/0,
        response_bridge/1
    ]).

%% This particular macro is really no longer needed the way it was when we were
%% using tuple calls, but I like the way the macros are highlighted when
%% a call uses ?BRIDGE, so I'm keeping it that way.
-define(BRIDGE, bridge()).

%%% REQUEST AND RESPONSE BRIDGE %%%

bridge() ->
    Context=context(),
    Context#context.bridge.

bridge(Bridge) ->
    Context = context(),
    context(Context#context{bridge=Bridge}).

in_request() ->
    %% If we have a context set and it is a #context{} tuple, then we are in a
    %% request.
    is_record(context(), context).

socket() ->
    sbw:socket(?BRIDGE).

path() ->
    sbw:path(?BRIDGE).

protocol() ->
    sbw:protocol(?BRIDGE).

uri() ->
    sbw:uri(?BRIDGE).

url() ->
    Protocol = wf:to_list(protocol()),
    Host = header(host),
    Uri = uri(),
    Protocol ++ "://" ++ Host ++ Uri.

peer_ip() ->
    sbw:peer_ip(?BRIDGE).


peer_ip(Proxies) ->
    peer_ip(Proxies,x_forwarded_for).

peer_ip(Proxies,ForwardedHeader) ->
    ConnIP = peer_ip(),
    case header(ForwardedHeader) of
        undefined -> ConnIP;
        RawForwardedIP ->
            ForwardedIP = wf_convert:parse_ip(RawForwardedIP),
            DoesIPMatch = fun(Proxy) ->
                wf_convert:parse_ip(Proxy) =:= ConnIP
            end,
            case lists:any(DoesIPMatch,Proxies) of
                true -> ForwardedIP;
                false -> ConnIP
            end
    end.

request_method() ->
    case sbw:request_method(?BRIDGE) of
        'GET'       -> get;
        get         -> get;
        'POST'      -> post;
        post        -> post;
        'DELETE'    -> delete;
        delete      -> delete;
        'PUT'       -> put;
        put         -> put;
        'TRACE'     -> trace;
        trace       -> trace;
        'HEAD'      -> head;
        head        -> head;
        'CONNECT'   -> connect;
        connect     -> connect;
        'OPTIONS'   -> options;
        options     -> options;
        Other -> list_to_existing_atom(string:to_lower(wf:to_list(Other)))
    end.

request_body() ->
    sbw:request_body(?BRIDGE).

status_code() ->
    sbw:get_status_code(?BRIDGE).

status_code(StatusCode) ->
    Bridge2 = sbw:set_status_code(StatusCode,?BRIDGE),
    bridge(Bridge2),
    ok.

content_type(ContentType) ->
    header("Content-Type", ContentType).

content_type() ->
    case sbw:get_response_header(<<"content-type">>,?BRIDGE) of
        undefined -> "text/html";
        ContentType -> ContentType
    end.

download_as(Filename0) ->
    Filename = wf_convert:url_encode(Filename0),
    header("Content-Disposition", "attachment; filename=\"" ++ Filename ++ "\"").

headers() ->
    sbw:headers(?BRIDGE).

header(Header) ->
    sbw:header(Header, ?BRIDGE).

header(Header, Value) ->
    Bridge2 = sbw:set_header(Header, Value, ?BRIDGE),
    bridge(Bridge2),
    ok.

-spec encoding(Encoding :: encoding()) -> ok.
encoding(Encoding) ->
    Context = context(),
    context(Context#context { encoding=Encoding }).

encoding() ->
    Context = context(),
    Context#context.encoding.


%%% TRANSIENT CONTEXT %%%

anchor(Anchor) ->
    Context = context(),
    context(Context#context { anchor=Anchor }).

anchor() ->
    Context = context(),
    Context#context.anchor.

data() ->
    Context = context(),
    Context#context.data.

data(Data) ->
    Context = context(),
    context(Context#context { data = Data }).

clear_data() ->
    Context = context(),
    context(Context#context { data = [] }).

-spec add_action(Priority :: wire_priority(), Action :: actions()) -> ok.
add_action(Priority, Action) when ?IS_ACTION_PRIORITY(Priority) ->
    ActionQueue = action_queue(),
    NewActionQueue = wf_action_queue:in(Priority, Action, ActionQueue),
    action_queue(NewActionQueue).

actions() ->
    ActionQueue = action_queue(),
    Actions = wf_action_queue:all(ActionQueue),
    clear_action_queue(),
    Actions.

-spec next_action() -> {ok, actions()} | empty.
next_action() ->
	ActionQueue = action_queue(),
	case wf_action_queue:out(ActionQueue) of
		{ok, Action, NewActionQueue} ->
            action_queue(NewActionQueue),
            {ok, Action};
		{error, empty} ->
            empty
	end.

action_queue() ->
	Context = context(),
	Context#context.action_queue.

action_queue(ActionQueue) ->
    Context = context(),
    context(Context#context { action_queue = ActionQueue }).

clear_action_queue() ->
    action_queue(new_action_queue()).

new_action_queue() ->
    wf_action_queue:new().

%%% PAGE CONTEXT %%%

page_context() ->
    Context = context(),
    Context#context.page_context.

page_context(PageContext) ->
    Context = context(),
    context(Context#context { page_context = PageContext }).

series_id() ->
    Page = page_context(),
    Page#page_context.series_id.

series_id(SeriesID) ->
    Page = page_context(),
    page_context(Page#page_context { series_id = SeriesID }).

page_module() -> 
    Page = page_context(),
    Page#page_context.module.

page_module(Module) ->
    Page = page_context(),
     page_context(Page#page_context { module = Module }).

entry_point() ->
    Page = page_context(),
    Page#page_context.entry_point.

entry_point(EntryPoint) ->
    Page = page_context(),
    page_context(Page#page_context { entry_point = EntryPoint}).

path_info() -> 
    Page = page_context(),
    Page#page_context.path_info.

path_info(PathInfo) ->
    Page = page_context(),
    page_context(Page#page_context { path_info = PathInfo }).

async_mode() ->
    Page = page_context(),
    Page#page_context.async_mode.

async_mode(AsyncMode) ->
    Page = page_context(),
    page_context(Page#page_context { async_mode=AsyncMode }).


%%% EVENT CONTEXT %%%

event_context() ->
    Context = context(),
    Context#context.event_context.

event_context(EventContext) ->
    Context = context(),
    context(Context#context { event_context = EventContext }).

type() ->
    Context = context(),
    Context#context.type.

type(Type) -> % either first_request, postback_request, postback_websocket, or static_file
    Context = context(),
    context(Context#context { type = Type }).

event_module() ->
    Event = event_context(),
    Event#event_context.module.

event_module(Module) ->
    Event = event_context(),
    event_context(Event#event_context { module = Module }).

event_tag() ->
    Event = event_context(),
    Event#event_context.tag.

event_tag(Tag) ->
    Event = event_context(),
    event_context(Event#event_context { tag = Tag }).

event_validation_group() ->
    Event = event_context(),
    Event#event_context.validation_group.

event_validation_group(ValidationGroup) ->
    Event = event_context(),
    event_context(Event#event_context { validation_group = ValidationGroup }).

event_handle_invalid() ->
    Event = event_context(),
    Event#event_context.handle_invalid.

event_handle_invalid(HandleInvalid) ->
    Event = event_context(),
    event_context(Event#event_context { handle_invalid = HandleInvalid }).

%%% HANDLERS %%%

handlers() ->
    Context = context(),
    Context#context.handler_list.

handlers(Handlers) ->
    Context = context(),
    context(Context#context { handler_list = Handlers }).

handler(HandlerName) ->
    Handlers = handlers(),
    case lists:keyfind(HandlerName, #handler_context.name, Handlers) of
        false -> undefined;
        HandlerContext -> HandlerContext
    end.

restore_handler(NewHandler) ->
    Handlers = handlers(),
    NewHandlers = [maybe_restore_handler(H, NewHandler) || H <- Handlers],
    handlers(NewHandlers).

maybe_restore_handler(Orig = #handler_context{name=Name}, New = #handler_context{name=Name}) ->
    New#handler_context{config=Orig#handler_context.config};
maybe_restore_handler(Orig, _New) ->
    Orig.

%% MAYBE DO THIS?
%%serializable_handlers() ->
%%    [H || H <- handlers(), is_handler_serializable(H)].
%%
%%is_handler_serializable(#handler_context{module=Module}) ->
%%    case erlang:function_exported(Module, is_serializable, 0) of
%%        true -> Module:is_serializable();
%%        false -> true
%%    end.

%%% CONTEXT CONSTRUCTION %%%

init_context(Bridge) ->
    % Create the new context using the default handlers.
    Context = #context {
        bridge = Bridge,
        page_context = #page_context { series_id = wf:temp_id() },
        event_context = #event_context {},
        action_queue = new_action_queue(),		
        handler_list = [
            % Core handlers...
            make_handler(config_handler, default_config_handler), 
            make_handler(process_registry_handler, nprocreg_registry_handler),
            make_handler(cache_handler, default_cache_handler), 
            make_handler(query_handler, default_query_handler),
            make_handler(crash_handler, default_crash_handler),

            % Stateful handlers...
            make_handler(session_handler, simple_session_handler), 
            make_handler(state_handler, default_state_handler), 
            make_handler(identity_handler, default_identity_handler), 
            make_handler(role_handler, default_role_handler), 

            % Handlers that possibly redirect...
            make_handler(route_handler, dynamic_route_handler), 
            make_handler(security_handler, default_security_handler)
        ]
    },
    context(Context).

make_handler(Name, Module) -> 
    #handler_context { 
        name=Name,
        module=Module,
        state=[]
    }.


%%% GET AND SET CONTEXT %%%
% Yes, the context is stored in the process dictionary. It makes the Nitrogen 
% code much cleaner. Trust me.
context() -> 
    get(context).
context(Context) -> 
    put(context, Context).

%% for debugging. Remove when ready
increment(Key) ->
    case get(Key) of
        undefined -> put(Key, 1);
        V -> io:format("~p=~p~n",[Key, V+1]), put(Key, V+1)
    end.

%% Kept for backwards compatibility with nitrogen 2.2 and below (and
%% simple_bridge 1.x)
request_bridge() ->
    bridge().

request_bridge(Bridge) ->
    bridge(Bridge).

response_bridge() ->
    bridge().

response_bridge(Bridge) ->
    bridge(Bridge).
