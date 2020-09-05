% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2020 Jesse Gumm
% See MIT-LICENSE for licensing information.

-module (element_delay_body).
-include("wf.hrl").
-export([
	reflect/0,
	transform_element/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, delay_body).

-spec transform_element(#delay_body{}) -> body().
transform_element(#delay_body{delegate=Delegate, tag=Tag, delay=Delay, placeholder=Placeholder}) when is_integer(Delay) ->
    ID = wf:temp_id(),
    queue_delayed_body(Delegate, ID, Tag, Delay),
    #span{id=ID, body=Placeholder}.

queue_delayed_body(Delegate, ID, Tag, Delay) ->
    %?PRINT(status(Delay)),
    add_to_queue(Delegate, ID, Tag, Delay),
    maybe_wire_postback(Delay).

wire_key(Delay) ->
    {element_delay_body_wired, Delay}.

tag_key(Delay) ->
    {element_delay_tags, Delay}.

add_to_queue(Delegate, ID, Tag, Delay) ->
    Key = tag_key(Delay),
    Queue = wf:state_default(Key, queue:new()),
    NewQ = queue:in({ID, Delegate, Tag}, Queue),
    %?PRINT(NewQ),
    wf:state(Key, NewQ).

%status(Delay) ->
%    {wf:state(wire_key(Delay)), wf:state(tag_key(Delay))}.

maybe_wire_postback(Delay) ->
    Key = wire_key(Delay),
    case wf:state_default(Key, false) of
        true ->
            %wf:info("we've already wired ~p, so skipping",[Delay]),
            do_nothing;
        _ -> 
            wf:state(Key, true),
            wf:defer(#event{type=timer, delay=Delay, delegate=?MODULE, postback={do_delay_actions, Delay}})
    end.

reset_wire_key(Delay) ->
    Key = wire_key(Delay),
    wf:state(Key, false).

reset_tag_key(Delay) ->
    Key = tag_key(Delay),
    wf:state(Key, queue:new()).

event({do_delay_actions, Delay}) ->
    %wf:info("Doing Delay Actions: ~p"),
    reset_wire_key(Delay), % we reset this in case we have to queue another action with the same timeout with a later postback
    TagKey = tag_key(Delay),
    NewQ = queue:new(),
    TagQueue = wf:state_default(TagKey, NewQ),
    Tags = queue:to_list(TagQueue),
    lists:foreach(fun({ID, Delegate, Tag}) ->
        Body = Delegate:delay_body_event(Tag),
        wf:replace(ID, Body)
    end, Tags),
    reset_tag_key(Delay).
    %?PRINT(status(Delay)).
