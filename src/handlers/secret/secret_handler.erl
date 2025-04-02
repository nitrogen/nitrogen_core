% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2025 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(secret_handler).
-include("wf.hrl").
-export ([
    get_value/2
]).

-callback init(         handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback finish(       handler_config(),
                        handler_state()) -> {ok, handler_state()}.
-callback get_value(    Key :: term(),
                        DefaultValue :: term(),
                        handler_config(),
                        handler_state()) -> term().

-spec get_value(Key :: term(), DefaultValue :: term()) -> term().
get_value(Key, DefaultValue) -> 
    _Value= wf_handler:call_readonly(secret_handler, get_value, [Key, DefaultValue]).
