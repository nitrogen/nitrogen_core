%% vim: ts=4 sw=4 et
-module(action_before_postback).
-include("wf.hrl").
-export([render_action/1]).

render_action(#before_postback{script=Script}) ->
    wf:f("Nitrogen.$before_postback(function(){ ~ts });",[Script]).
