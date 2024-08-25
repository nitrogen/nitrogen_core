% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2016 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (action_js_fun).
-include("wf.hrl").
-export([
	render_action/1,
	js_fun/1,
	js_fun/2
]).

render_action(#js_fun{function=Fun0, args=Args0}) ->
	Fun = wf:to_list(Fun0),
	Args1 = [[handle_arg(Arg)] || Arg <- Args0],
	Args = wf:join(Args1, ","),
	[Fun,"(",Args,")"].

js_fun(Fun) ->
	js_fun(Fun, []).

js_fun(Fun, Args) when is_list(Args) ->
	wf:wire(#js_fun{function=Fun, args=Args}).

handle_arg(null) ->
    <<"null">>;
handle_arg(undefined) ->
    <<"null">>;
handle_arg(true) ->
    <<"true">>;
handle_arg(false) ->
    <<"false">>;
handle_arg(X) when is_integer(X); is_float(X) ->
    wf:to_binary(X);
handle_arg(X) when ?IS_STRING(X); is_binary(X); is_atom(X) ->
    [<<"\"">>,wf:js_escape(wf:to_unicode_binary(X)), <<"\"">>];
handle_arg(X = #js_fun{}) ->
    render_action(X).
    %% actions render as strings that process into javascript.
    %[<<"(function() { return ">>,X, <<"}())">>].
    %X.
