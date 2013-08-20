% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2012 Milan Svoboda
% See MIT-LICENSE for licensing information.

-module (action_make_readonly).
-include("wf.hrl").
-export([
    render_action/1
]).

-spec render_action(#make_readonly{}) -> actions().
render_action(#make_readonly{target=Target}) ->
	#script{script=wf:f(<<"obj('~s').prop('readonly',true);">>, [Target])}.

