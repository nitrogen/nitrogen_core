% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_function).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1
]).

% The 'function' attribute is an Erlang function of arity 0 that returns [Elements].
%
% Elements can either be a String, a binary, more Nitrogen elements, or a combination thereof. 
%
% Alternatively, the 'function' attribute can be a list
% of functions having the properties above. The first
% one that exists and returns a value (not undefined) will
% be used.

-spec reflect() -> [atom()].
reflect() -> record_info(fields, function_el).

-spec render_element(#function_el{}) -> body().
render_element(Record) ->
    Functions = lists:flatten([Record#function_el.function]),
    call_next_function(Functions).

-spec call_next_function([fun()]) -> body().
call_next_function([]) -> []; 
call_next_function([F|Functions]) ->
    % Call the function. If it provides results, then return it, 
    % Otherwise, call the next function.
    case F() of
        undefined -> call_next_function(Functions);
        Elements  -> Elements
    end.
