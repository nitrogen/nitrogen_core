% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% See MIT-LICENSE for licensing information.

-module (postback_handler).
-include("wf.hrl").
-export([ postback_request/0 ]).

-callback init(handler_config(),
               handler_state()) -> {ok, handler_state()}.

-callback finish(handler_config(),
                 handler_state()) -> {ok, handler_state()}.

-callback postback_request(handler_config(),
                           handler_state()) -> {ok, handler_state()}.


postback_request() ->
    _Value = wf_handler:call_readonly(postback_handler, postback_request, []).
