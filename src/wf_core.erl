% vim: sw=4 ts=4 et
-module (wf_core).
-include("wf.hrl").
-export ([
    run/0,
    init_websocket/1,
    run_websocket/1,
    run_websocket_comet/0,
    run_websocket_crash/3,
    serialize_context/0
]).

% nitrogen_core - 
% --
% Render a single Nitrogen page or inline application. This can be called
% from other Erlang web frameworks or resource servers, such as WebMachine, 
% Erlang Web, ErlyWeb, etc.

run() ->
    Bridge = wf_context:bridge(),
    try 
        case Bridge:error() of
            none -> run_catched();
            Other -> 
                Message = wf:f("Errors: ~p~n", [Other]),
                Bridge1 = Bridge:set_response_data(Message),
                Bridge1:build_response()
        end
    catch
        Type : Error -> 
            run_crash(Bridge, Type, Error, erlang:get_stacktrace())
    end.

    

run_crash(Bridge, Type, Error, Stacktrace) ->
    try
        case wf_context:type() of
            first_request       -> run_crashed_first_request(Type, Error, Stacktrace);
            static_file         -> run_crashed_first_request(Type, Error, Stacktrace);
            postback_request    -> run_crashed_postback_request(Type, Error, Stacktrace);
            _                   -> run_crashed_first_request(Type, Error, Stacktrace)
        end,
        finish_dynamic_request()
    catch
        Type2:Error2 ->
            ?LOG("~p~n", [{error, Type2, Error2, erlang:get_stacktrace()}]),
            Bridge1 = sbw:set_status_code(500, Bridge),
            Bridge2 = sbw:set_response_data("Internal Server Error", Bridge1),
            sbw:build_response(Bridge2)
    end.

init_websocket(SerializedPageContext) ->
    deserialize_websocket_context(SerializedPageContext),
    wf_context:async_mode({websocket, self()}),
    call_init_on_handlers().

run_websocket_crash(Type, Error, Stacktrace) ->
    try
        crash_handler:postback_request(Type, Error, Stacktrace),
        run_websocket_comet()
    catch Type2:Error2 ->
        ?LOG("~p~n", [{error_in_crash_handler, Type2, Error2, erlang:get_stacktrace()}]),
        "Nitrogen.$console_log('crash_handler crashed in websocket');"
    end.

run_websocket_comet() ->
    wf_context:type(postback_websocket),
    _ToSend = finish_websocket_request().

run_websocket(Data) ->
    wf_event:update_context_with_websocket_event(Data),
    query_handler:set_websocket_params(Data),
    run_postback_request(),
    _ToSend = finish_websocket_request().

run_catched() ->
    deserialize_request_context(),
    call_init_on_handlers(),
    wf_event:update_context_with_event(wf:q(eventContext)),
    case wf_context:type() of
        first_request    -> 
            run_first_request(), 
            finish_dynamic_request();
        postback_request -> 
            run_postback_request(), 
            finish_dynamic_request();
        static_file      -> 
            finish_static_request()
    end.

finish_dynamic_request() ->
    Elements = wf_context:data(),
    wf_context:clear_data(),
    {ok, Html} = wf_render_elements:render_elements(Elements),
	{ok, Javascript} = wf_render_actions:render_action_queue(),

    call_finish_on_handlers(),

    StateScript = serialize_context(),
    JavascriptFinal = [StateScript, Javascript],

    case wf_context:type() of
        first_request       -> build_first_response(Html, JavascriptFinal);
        postback_request    -> build_postback_response(JavascriptFinal);
        _                   -> build_first_response(Html, JavascriptFinal)
    end.

finish_websocket_request() ->
    ContextData = wf_context:data(),
    wf_context:clear_data(),
    finish_websocket_request(ContextData).

finish_websocket_request(Empty)
        when Empty==undefined; Empty==[]; Empty == <<>> ->
    {ok, Javascript} = wf_render_actions:render_action_queue(),
    StateScript = serialize_context(),
    [StateScript, Javascript];
finish_websocket_request(Data) ->
    Data.

finish_static_request() ->
    Path = wf_context:path_info(),
    build_static_file_response(Path).

%%% SERIALIZE AND DESERIALIZE STATE %%%

% serialize_context_state/0 -
% Serialize part of Context and send it to the browser
% as Javascript variables.
serialize_context() ->
    % Get page context...
    Page = wf_context:page_context(),

    % Get handler context, but don't serialize the config.
    StateHandler = wf_context:handler(state_handler),
    SerializedContextState = wf_pickle:pickle([Page, StateHandler]),
    wf:f("Nitrogen.$set_param('pageContext', '~s');~n", [SerializedContextState]).

deserialize_request_context() ->
    Bridge = wf_context:bridge(),
    SerializedPageContext = sbw:post_param(<<"pageContext">>, Bridge),
    deserialize_context(SerializedPageContext).

deserialize_websocket_context(SerializedPageContext) ->
    deserialize_context(SerializedPageContext).


% deserialize_context/1 -
% Updates the context with values that were stored
% in the browser by serialize_context_state/1.
deserialize_context(SerializedPageContext) ->
    OldStateHandler = wf_context:handler(state_handler),

    % Deserialize page_context and handler_list if available...
    [PageContext, NewStateHandler] = case SerializedPageContext of
        undefined -> [wf_context:page_context(), OldStateHandler];
        Other -> wf_pickle:depickle(Other)
    end,

    wf_context:page_context(PageContext),
    wf_context:restore_handler(NewStateHandler),
    ok.

%%% SET UP AND TEAR DOWN HANDLERS %%%

% init_handlers/1 - 
% Handlers are initialized in the order that they exist in #context.handlers. The order
% is important, as some handlers may depend on others being initialize. For example, 
% the session handler may use the cookie handler to get or set the session cookie.
call_init_on_handlers() ->
    Handlers = wf_context:handlers(),
    [wf_handler:call(X#handler_context.name, init) || X <- Handlers],
    ok.

% finish_handlers/1 - 
% Handlers are finished in the order that they exist in #context.handlers. The order
% is important, as some handlers should finish after others. At the very least,
% the 'render' handler should go last to make sure that it captures all changes
% put in place by the other handlers.
call_finish_on_handlers() ->
    Handlers = wf_context:handlers(),
    [wf_handler:call(X#handler_context.name, finish) || X <- Handlers],
    ok.	


%%% FIRST REQUEST %%%

run_first_request() ->
    Module = wf_context:event_module(),
    {module, Module} = code:ensure_loaded(Module),
    EntryPoint = wf_context:entry_point(),
    Data = run_entry_point(Module, EntryPoint),
    wf_context:data(Data).

run_entry_point(_Module, Fun) when is_function(Fun, 0) ->
    Fun();
run_entry_point(Module, Fun) when is_atom(Fun) ->
    Module:Fun().

run_crashed_first_request(Type, Error, Stacktrace) ->
    Data = crash_handler:first_request(Type, Error, Stacktrace),
    wf_context:data(Data).

%%% POSTBACK REQUEST %%%

run_postback_request() ->
    Module = wf_context:event_module(),
    Tag = wf_context:event_tag(),
    HandleInvalid = wf_context:event_handle_invalid(),
    {ok, IsValid} = wf_validation:validate(),
    call_postback_event(IsValid, HandleInvalid, Module, Tag).

call_postback_event(_Valid=true, _HandleInvalid, Module, Tag) ->
    Module:event(Tag);
call_postback_event(_Valid=false, _HandleInvalid=true, Module, Tag) ->
    Module:event_invalid(Tag);
call_postback_event(_Valid, _HandleInvalid, _, _) ->
    ok.

run_crashed_postback_request(Type, Error, Stacktrace) ->
    crash_handler:postback_request(Type, Error, Stacktrace).

%%% BUILD THE RESPONSE %%%

build_static_file_response(Path) ->
    Bridge = wf_context:bridge(),
    Bridge1 = sbw:set_response_file(Path, Bridge),
    sbw:build_response(Bridge1).

build_first_response(Html, Script) ->
    % Update the output with any script...
    Html1 = replace_script(Script, Html),
    Html2 = unicode:characters_to_binary(Html1),

    % Update the response bridge and return.
    Bridge = wf_context:bridge(),
    Bridge1 = sbw:set_response_data(Html2, Bridge),
    sbw:build_response(Bridge1).

build_postback_response(Script) ->
    % Update the response bridge and return.
    Bridge = wf_context:bridge(),
    Script1 = unicode:characters_to_binary(Script),
    Bridge1 = sbw:set_response_data(Script1, Bridge),
    sbw:build_response(Bridge1).

replace_script(_,Html) when ?IS_STRING(Html) -> Html;
replace_script(Script, [script|T]) -> [Script|T];
%% For the mobile_script, it's necessary that it's inside the data-role attr,
%% and therefore must be escaped before it can be sent to the browser
replace_script(Script, [mobile_script|T]) -> [wf:html_encode(Script)|T];
replace_script(Script, [H|T]) -> [replace_script(Script, H)|replace_script(Script, T)];
replace_script(_, Other) -> Other.
