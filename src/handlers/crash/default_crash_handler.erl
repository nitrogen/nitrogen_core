-module(default_crash_handler).
-behaviour(crash_handler).
-include("wf.hrl").
-export([
	init/2,
	finish/2,
	first_request/5,
	postback_request/5
]).

init(_Config,State) ->
	{ok, State}.

finish(_Config, State) ->
	{ok, State}.

first_request(Type, Error, Stacktrace, _Config, _State) ->
	?WF_LOG("~p~n", [{error, Type, Error, Stacktrace}]),
	wf:status_code(500),
	"Internal Server Error".

postback_request(Type, Error, Stacktrace, _Config, _State) ->
	?WF_LOG("~p~n", [{error, Type, Error, Stacktrace}]),
	wf:status_code(500),
	wf:console_log("Postback Crashed. See console for details"),
	ok.
