% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (validation_handler).
-include("wf.hrl").
-export([attach/3, validate/2]).

-callback init(handler_config(),
               handler_state()) -> {ok, handler_state()}.

-callback finish(handler_config(),
                 handler_state()) -> {ok, handler_state()}.

-callback validate(id(),
                   text(),
                   handler_config(),
                   handler_state()) -> true | {false, actions()}.

-callback attach(Targetid :: id(),
                Field :: id(),
                validators(),
                handler_config(),
                handler_state()) -> {ok, handler_state()}.

validate(ID, Text) ->
    wf_handler:call(?MODULE, ?FUNCTION_NAME, [ID, Text]).


attach(Targetid, Field, Validators) ->
    wf_handler:call(?MODULE, ?FUNCTION_NAME, [Targetid, Field, Validators]).
