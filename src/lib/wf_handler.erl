% vim: sw=4 ts=4 et ft=erlang
-module (wf_handler).
-include("wf.hrl").
-export ([
    init/1,
    finish/1,
    call/2, 
    call/3,
    call_readonly/2, 
    call_readonly/3,
    set_handler/2,
    set_handler/3,
    set_global_handler/2,
    set_global_handler/3,
    get_handler_name/1,
    default_global_handlers/0,
    default_request_handlers/0,
    handler_order/0,
    handler_default/1
]).

-spec init(#handler_context{}) -> ok.
init(H = #handler_context{module=Module, config=Config, state=State}) ->
    {ok, NewState} = Module:init(Config, State),
    NewH = H#handler_context{state=NewState},
    append_handler_state(NewH).

-spec finish(#handler_context{}) -> ok.
finish(H = #handler_context{}) ->
    call(H, finish, []).

% Helper function to call a function within a handler.
% Returns ok or {ok, Value}.
call(Name, FunctionName) -> call(Name, FunctionName, []).

% Helper function to call a function within a handler.
% Returns ok or {ok, Value}.
call(Name, FunctionName, Args) when is_atom(Name) ->
    % Get the handler and state from the context. Then, call
    % the function, passing in the Args and State.
    Handler = get_handler(Name),
    call(Handler, FunctionName, Args);

call(#handler_context{ name=Name, module=Module, config=Config, state=State }, FunctionName, Args) ->
    Result = erlang:apply(Module, FunctionName, Args ++ [Config, State]),

    % Result will be {ok, State}, {ok, Value1, State}, or {ok, Value1, Value2, State}.
    % Update the context with the new state.
    case Result of
        {ok, NewState} -> 
            update_handler_state(Name, State, NewState),
            ok;

        {ok, Value, NewState} ->
            update_handler_state(Name, State, NewState),
            {ok, Value};

        {ok, Value1, Value2, NewState} ->
            update_handler_state(Name, State, NewState),
            {ok, Value1, Value2}
    end.

call_readonly(Name, FunctionName) -> call_readonly(Name, FunctionName, []).

call_readonly(Name, FunctionName, Args) ->
    % Get the handler and state from the context. Then, call
    % the function, passing in the Args with State appended.
    #handler_context { module=Module, config=Config, state=State } = get_handler(Name),
    erlang:apply(Module, FunctionName, Args ++ [Config, State]).

set_handler(Module, Config) ->
    case code:ensure_loaded(Module) of
        {module, Module} -> ok;
        Error -> throw({unable_to_load_handler, {module, Module}, {error, Error}})
    end,
    Name = get_handler_name(Module),
    set_handler(Name, Module, Config).

get_handler_name(Module) ->
    % Get the module's behavior...
    L = Module:module_info(attributes),
    case proplists:get_value(behaviour, L) of
        [N] -> N;
        _      -> throw({must_define_a_nitrogen_behaviour, Module})
    end.


% set_handler/3
% Set the configuration for a handler.
set_handler(Name, Module, Config) ->
    Handlers = wf_context:handlers(),
    OldHandler = lists:keyfind(Name, 2, Handlers),
    NewHandler = OldHandler#handler_context { module=Module, config=Config },
    NewHandlers = lists:keyreplace(Name, 2, Handlers, NewHandler),
    wf_context:handlers(NewHandlers).	

set_global_handler(Module, Config) ->
    nitrogen_handler_srv:set_handler(Module, Config).

set_global_handler(Name, Module, Config) ->
    nitrogen_handler_srv:set_handler(Name, Module, Config).

% get_handler/2 - 
% Look up a handler in a context. Return {ok, HandlerModule, State}
get_handler(Name) -> 
    try wf_handler_mapper:lookup(Name) of
        context ->
            get_request_handler(Name);
        Handler when is_record(Handler, handler_context) ->
            Handler
    catch
        error:undef ->
            erlang:error("wf_handler_mapper module hasn't been built. This likely means nitrogen_core needs to be started")
    end.

get_request_handler(Name) ->
    Handlers = wf_context:handlers(),
    case lists:keyfind(Name, 2, Handlers) of
        Handler when is_record(Handler, handler_context) -> 
            Handler;
        false -> 
            throw({handler_not_found_in_context, Name, Handlers})
    end.

update_handler_state(_Name, _OrigState, no_change) ->
    ok;
update_handler_state(_Name, OrigState, OrigState) ->
    ok;
update_handler_state(Name, _OrigState, State) ->
    Handlers = wf_context:handlers(),
    OldHandler = lists:keyfind(Name, 2, Handlers),
    NewHandler = OldHandler#handler_context { state=State },
    NewHandlers = lists:keyreplace(Name, 2, Handlers, NewHandler),
    wf_context:handlers(NewHandlers).

append_handler_state(State) ->
    Handlers = wf_context:handlers(),
    wf_context:handlers(Handlers ++ [State]).

default_global_handlers() ->
    [
        config_handler,
        log_handler,
        process_registry_handler,
        cache_handler,
        query_handler,
        crash_handler,
        websocket_handler,
        session_handler,
        identity_handler,
        role_handler,
        route_handler,
        security_handler,
        postback_handler
    ].

default_request_handlers() ->
    [
        session_handler,
        state_handler
    ].

handler_order() ->
    [
        config_handler,
        log_handler,
        process_registry_handler,
        cache_handler,
        query_handler,
        crash_handler,
        websocket_handler,
        session_handler,
        state_handler,
        identity_handler,
        role_handler,
        route_handler,
        security_handler,
        postback_handler
    ].


handler_default(config_handler) ->      default_config_handler;
handler_default(log_handler) ->         default_log_handler;
handler_default(process_registry_handler) -> nprocreg_registery_handler;
handler_default(cache_handler) ->       default_cache_handler;
handler_default(query_handler) ->       defaut_query_handler;
handler_default(crash_handler) ->       default_crash_handler;
handler_default(websocket_handler) ->   default_websocket_handler;
handler_default(session_handler) ->     canister_session_handler;
handler_default(state_handler) ->       default_state_handler;
handler_default(identity_handler) ->    default_identity_handler;
handler_default(role_handler) ->        default_role_handler;
handler_default(route_handler) ->       default_route_handler;
handler_default(security_handler) ->    default_security_handler;
handler_default(postback_handler) ->    default_postback_handler.
