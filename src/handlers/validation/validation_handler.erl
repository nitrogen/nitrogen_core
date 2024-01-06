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

-callback validate(handler_config(),
                 handler_state(),
                 id(),
                 text()) -> true | {false, Actions}.

-callback attach(handler_config(),
                 handler_state(),
                 Targetid :: id(),
                 Field :: id(),
                 validators()) -> {ok, handler_state()}.


postback_request() ->
    _Value = wf_handler:call_readonly(postback_handler, postback_request, []).
