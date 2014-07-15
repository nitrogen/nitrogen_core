% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (wf_context).
-include("wf.hrl").

-export([
        bridge/0,
        bridge/1,

        socket/0,

        peer_ip/0,
        peer_ip/1,
        peer_ip/2,

        request_body/0,
        status_code/0,
        status_code/1,

        content_type/1,
        headers/0,
        header/1,
        header/2,

        cookies/0,
        cookie/1,
        cookie_default/2,

        cookie/2,
        cookie/4,
        delete_cookie/1,

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

        init_context/1,
        make_handler/2,

        context/0,
        context/1
    ]).

%% Exports for backwards compatibility
-export([
        request_bridge/0,
        request_bridge/1,
        response_bridge/0,
        response_bridge/1
    ]).

-define(BRIDGE, (bridge())).

%%% REQUEST AND RESPONSE BRIDGE %%%

bridge() ->
    Context=context(),
    Context#context.bridge.

bridge(Bridge) ->
    Context = context(),
    context(Context#context{bridge=Bridge}).

socket() ->
    ?BRIDGE:socket().

path() ->
    Req = request_bridge(),
    Req:path().

protocol() ->
    Req = request_bridge(),
    Req:protocol().

uri() ->
    Req = request_bridge(),
    Req:uri().

url() ->
    Protocol = wf:to_list(protocol()),
    Host = header(host),
    Uri = uri(),
    Protocol ++ "://" ++ Host ++ Uri.

peer_ip() ->
    ?BRIDGE:peer_ip().

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


request_body() ->
    ?BRIDGE:request_body().

status_code() ->
    ?BRIDGE:status_code().

status_code(StatusCode) ->
    bridge(?BRIDGE:set_status_code(StatusCode)),
    ok.

content_type(ContentType) ->
    bridge(?BRIDGE:set_header("Content-Type", ContentType)),
    ok.

headers() ->
    ?BRIDGE:headers().

header(Header) ->
    ?BRIDGE:header(Header).

header(Header, Value) ->
    bridge(?BRIDGE:set_header(Header, Value)),
    ok.

cookies() ->
    ?BRIDGE:cookies().

cookie(Cookie) ->
    ?BRIDGE:cookie(Cookie).

cookie_default(Cookie,DefaultValue) ->
    case cookie(Cookie) of
        undefined -> DefaultValue;
        Value -> Value
    end.

cookie(Cookie, Value) ->
    bridge(?BRIDGE:set_cookie(Cookie, Value)).

cookie(Cookie, Value, Path, MinutesToLive) ->
    bridge(?BRIDGE:cookie(Cookie, Value, Path, MinutesToLive)),
    ok.

delete_cookie(Cookie) ->
    cookie(Cookie,"","/",0).


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
    NewActionQueue = wf_action_queue:clear(ActionQueue),
    action_queue(NewActionQueue),
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

type(Type) -> % either first_request, postback_request, or static_file
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
            make_handler(log_handler, default_log_handler),
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
context() -> get(context).
context(Context) -> put(context, Context).


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

