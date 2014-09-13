% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2014 Jesse Gumm
% See MIT-LICENSE for licensing information.
-module(nitrogen_smart_extensions).
-export([
	json/1
]).

json(EntryFun) ->
    wf:content_type("application/json"),
    Mod = wf_context:page_module(),
    Json = Mod:EntryFun(),
    _EncodedJson = wf:json_encode(Json).
