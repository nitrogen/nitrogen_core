% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (default_log_handler).
-behaviour (log_handler).
-include("wf.hrl").
-export ([
    init/2, 
    finish/2,
    info/3, 
    warning/3, 
    error/3
]).

-spec init(handler_config(), handler_state()) -> {ok, handler_state()}.
init(_Config, State) -> 
    {ok, State}.

-spec finish(handler_config(), handler_state()) -> {ok, handler_state()}.
finish(_Config, State) -> 
    {ok, State}.

-spec info(S :: string(), handler_config(), handler_state()) -> {ok, handler_state()}.
info(S, _Config, State) -> 
    ok = error_logger:info_msg(S ++ "\n"),
    {ok, State}.

-spec warning(S :: string(), handler_config(), handler_state()) -> {ok, handler_state()}.
warning(S, _Config, State) -> 
    ok = error_logger:warning_msg(S ++ "\n"),
    {ok, State}.

-spec error(S :: string(), handler_config(), handler_state()) -> {ok, handler_state()}.
error(S, _Config, State) -> 
    ok = error_logger:error_msg(S ++ "\n"),
    {ok, State}.
