% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (log_handler).
-compile({no_auto_import,[error/1]}).
-include("wf.hrl").
-export ([
    info/1, info/2,
    warning/1, warning/2,
    error/1, error/2
]).

-callback init(     handler_config(),
                    handler_state()) -> {ok, handler_state()}.
-callback finish(   handler_config(),
                    handler_state()) -> {ok, handler_state()}.
-callback info(     string(),
                    handler_config(),
                    handler_state()) -> {ok, handler_state()}.
-callback error(    string(),
                    handler_config(),
                    handler_state()) -> {ok, handler_state()}.
-callback warning(  string(),
                    handler_config(),
                    handler_state()) -> {ok, handler_state()}.

% Log an info-level message. Everything is functioning as usual.
info(String, Args) -> 
    ok = info(wf:f(String, Args)).

info(String) when is_binary(String) ->
    ok = info(binary_to_list(String));
info(String) -> 
    ok = wf_handler:call(log_handler, info, [String]).

% Log a warning-level message. If something is not corrected, then
% service could be interrupted in some way.
warning(String, Args) -> 
    ok = warning(wf:f(String, Args)).

warning(String) when is_binary(String) ->
    ok = warning(binary_to_list(String));
warning(String) -> 
    ok = wf_handler:call(log_handler, warning, [String]).

% Log an error-level message. Service has been interrupted in some way.
error(String, Args) -> 
    ok = error(wf:f(String, Args)).

error(String) when is_binary(String) ->
    ok = error(binary_to_list(String));
error(String) -> 
    ok = wf_handler:call(log_handler, error, [String]).

