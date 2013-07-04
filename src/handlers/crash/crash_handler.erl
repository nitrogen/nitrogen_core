% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyroght (c) 2013 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module(crash_handler).
-include("wf.hrl").
-export([
	behaviour_info/1,
	first_request/3,
	postback_request/3
]).

-spec first_request(ErrorType :: term(), Error :: term(), Stacktrace :: [erlang:stack_item()]) -> body().
%% @doc Called when an initial request's response crashes, and is used to
%% present friendly error message to the user instead of "Internal Server
%% Error"
first_request(ErrorType, Error, Stacktrace) ->
	_Body = wf_handler:call_readonly(crash_handler, first_request, [ErrorType, Error, Stacktrace]).

-spec postback_request(ErrorType :: term(), Error :: term(), Stacktrace :: [erlang:stack_item()]) -> ok.
%% @doc Called when a postback/comet/websocket response fails. Can be used to 
%% present better message to the user that simply silently failing.
%% For example: making a javascript alert informing the user that the request
%% failed.
postback_request(ErrorType, Error, Stacktrace) ->
	_Body = wf_handler:call_readonly(crash_handler, postback_request, [ErrorType, Error, Stacktrace]).

-spec behaviour_info(atom()) -> erlang:proplist() | undefined.
behaviour_info(callbacks) -> [
	{init, 2},
	{finish, 2},
	{first_request, 5},
	{postback_request, 5}
];

behaviour_info(_) -> undefined.


