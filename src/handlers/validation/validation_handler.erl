% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (validation_handler).
-include("wf.hrl").
-export([js_constructor/5, js_add_validator/3, js_add_validator/2,  required_js/0]).

-callback init(handler_config(),
               handler_state()) -> {ok, handler_state()}.

-callback finish(handler_config(),
                 handler_state()) -> {ok, handler_state()}.

%%-callback validate(id(),
%%                   text(),
%%                   handler_config(),
%%                   handler_state()) -> true | {false, actions()}.
%%
%%-callback attach(Targetid :: id(),
%%                Field :: id(),
%%                validators(),
%%                handler_config(),
%%                handler_state()) -> {ok, handler_state()}.


-callback js_constructor(TargetPath :: id(),
                  ValidationGroup :: atom() | text(),
                  ValidMessage :: text(),
                  On :: [validate_event()],
                  AttachTo :: undefined | id(),
                  handler_config(),
                  handler_state()) -> {ok, script(), handler_state()}.

-callback js_add_validator(Type :: atom(),
                           FailureMessage :: text(),
                           Options :: proplist() | map(),
                           handler_config(),
                           handler_state()) -> script().
    

-callback required_js(handler_config(),
                      handler_state()) -> text().


required_js() ->
    wf_handler:call(?MODULE, ?FUNCTION_NAME, []).

%%validate(ID, Text) ->
%%    case wf_handler:call(?MODULE, ?FUNCTION_NAME, [ID, Text]) of
%%        true -> true;
%%        {false, Actions} ->
%%            wf:wire(Actions)
%%    end.
%%
%%attach(Targetid, Field, Validators) ->
%%    wf_handler:call(?MODULE, ?FUNCTION_NAME, [Targetid, Field, Validators]).

js_constructor(TargetPath, ValidationGroup, ValidMessage, On, AttachTo) when is_atom(On) ->
    js_constructor(TargetPath, ValidationGroup, ValidMessage, [On], AttachTo);
js_constructor(TargetPath, ValidationGroup, ValidMessage, On, AttachTo) when is_list(On) ->
    {ok, Script} = wf_handler:call(?MODULE, ?FUNCTION_NAME, [TargetPath, ValidationGroup, ValidMessage, On, AttachTo]),
    Script.

js_add_validator(Type, FailureMessage) ->
    js_add_validator(Type, FailureMessage, []).

js_add_validator(Type, FailureMessage, Options) ->
    wf_handler:call_readonly(?MODULE, ?FUNCTION_NAME, [Type, FailureMessage, Options]).
