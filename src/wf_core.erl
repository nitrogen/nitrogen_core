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
        case sbw:error(Bridge) of
            none -> run_catched();
            Other -> 
                Message = wf:f("Errors: ~p~n", [Other]),
                Bridge1 = sbw:set_response_data(Message, Bridge),
                sbw:build_response(Bridge1)
        end
    catch
        exit:normal ->
            exit(normal);
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
        exit:normal ->
            exit(normal);
        Type2:Error2 ->
            ?LOG("Crash Handler Crashed:~n~p~n~nOriginal Crash:~n~p~n", [
                {Type2, Error2, erlang:get_stacktrace()},
                {Type, Error, Stacktrace}]),
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
    _ToSend = wf:to_unicode_binary(finish_websocket_request()).

run_websocket(Data) ->
    wf_context:type(postback_websocket),
    wf_event:update_context_with_websocket_event(Data),
    query_handler:set_websocket_params(Data),
    run_postback_request(),
    _ToSend = wf:to_unicode_binary(finish_websocket_request()).

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
    {ok, Html} = maybe_render_elements(Elements),
	{ok, Javascript} = wf_render_actions:render_action_queue(),

    call_finish_on_handlers(),

    StateScript = serialize_context(),
    JavascriptFinal = [StateScript, Javascript],

    case wf_context:type() of
        first_request       -> build_first_response(Html, JavascriptFinal);
        postback_request    -> build_postback_response(JavascriptFinal);
        _                   -> build_first_response(Html, JavascriptFinal)
    end.

maybe_render_elements(Elements = {sendfile, 0, _Size, _FullPath}) ->
    %% cowboy_simple_bridge can handle sending files directly
    {ok, Elements};
maybe_render_elements(Elements = {file, _Filename}) ->
    %% This will pass the {file,_} return to simple_bridge to serve a file
    %% directly
    {ok, Elements};
maybe_render_elements(Elements = {stream, Size, Fun}) when is_integer(Size), is_function(Fun) ->
    %% This is used for passing a {stream, Size, StreamFun} function to
    %% simple_bridge (currently only works with cowboy)
    {ok, Elements};
maybe_render_elements(Elements) ->
    %{Time, {ok, Html}} = timer:tc(wf_render_elements, render_elements, [Elements]),
    %io:format("Render Time: ~p~n",[Time]),
    %{ok, Html}.
    {ok, _Html} = wf_render_elements:render_elements(Elements).


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
% in the browser by serialize_context/1.
deserialize_context(undefined) ->
    %% If the serialized page context is undefined, don't do anything.
    ok;
deserialize_context(SerializedPageContext) ->
    % Deserialize page_context and handler_list if available...
    case wf_pickle:depickle(SerializedPageContext) of
        [PageContext, NewStateHandler] ->
            wf_context:page_context(PageContext),
            wf_context:restore_handler(NewStateHandler),
            ok;
        undefined ->
            exit({failure_to_deserialize_page_context, [
                {serialized_page_context, SerializedPageContext},
                {suggestion, "The most common cause of this is that "
                             "simple_cache is not started. Try running: "
                             "application:start(simple_cache)."}]})
    end.


%%% SET UP AND TEAR DOWN HANDLERS %%%

% init_handlers/1 - 
% Handlers are initialized in the order that they exist in #context.handlers. The order
% is important, as some handlers may depend on others being initialize. For example, 
% the session handler may use the cookie handler to get or set the session cookie.
% TODO: Re-evaluate handlers into some form of middleware layer or something.
% Allowing us to pass handler contexts from one handler to another and limit
% the number of process dict sets and gets
call_init_on_handlers() ->
    %% Get initial handlers
    Handlers = wf_context:handlers(),
    %% Clear Handler list, to re-initiate in order
    wf_context:handlers([]),
    %% Re-initiate handlers in order, appending them back to the handler list as we go
    [wf_handler:init(X) || X <- Handlers],
    ok.

% finish_handlers/1 - 
% Handlers are finished in the order that they exist in #context.handlers. The order
% is important, as some handlers should finish after others.
call_finish_on_handlers() ->
    [wf_handler:finish(X) || X <- wf_context:handlers()],
    ok.	


%%% FIRST REQUEST %%%

run_first_request() ->
    Module = wf_context:event_module(),
    {module, Module} = wf_utils:ensure_loaded(Module),
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
    Html2 = encode(Html1),

    % Update the response bridge and return.
    Bridge = wf_context:bridge(),
    Bridge1 = sbw:set_response_data(Html2, Bridge),
    sbw:build_response(Bridge1).

build_postback_response(Script) ->
    % Update the response bridge and return.
    Bridge = wf_context:bridge(),
    %% Encoding for a postback request will be utf8
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

encode(Text) ->
    Encoding = wf_context:encoding(), 
    encode(Encoding, Text).

-spec encode(encoding(), iolist()) -> iolist().
encode(none, Body) ->
    Body;
encode(unicode, Body) ->
    case unicode:characters_to_binary(Body) of
        {error, Encoded, Rest} ->
            wf:warning("Unable to encode Unicode: ~p", [Rest]),
            [Encoded, Rest];
        {incomplete, Encoded, Rest} ->
            wf:warning("Incomplete Unicode: ~p", [Rest]),
            [Encoded, Rest];
        Binary when is_binary(Binary) ->
            Binary
    end;
encode({Mod, Fun}, Body) ->
    Mod:Fun(Body);
encode(Fun, Body) when is_function(Fun, 1) ->
    Fun(Body);
encode(auto, Body) ->
    Encoding = case config_encoding() of
        undefined ->
            ContentType = wf_context:content_type(),
            encoding_by_content_type(wf:to_binary(ContentType));
        Other ->
            Other
    end,
    encode(Encoding, Body).

config_encoding() ->
    wf:config_default(default_encoding, undefined).

%% Please note that this is naive, in that it doesn't even check content-type
%% for charset. This is done for speed (to avoid running a regex on each
%% request).
encoding_by_content_type(<<"text/",_/binary>>) ->
    unicode;
encoding_by_content_type(<<"application/javascript">>) ->
    unicode;
encoding_by_content_type(<<"application/json">>) ->
    unicode;
encoding_by_content_type(<<"application/csv">>) ->
    unicode;
encoding_by_content_type(_) ->
    none.
