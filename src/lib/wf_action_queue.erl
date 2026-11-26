%% vim: ts=4 sw=4 et
%% This is a basic 3-priority queue.
-module(wf_action_queue).
-include("wf.hrl").
-export([
    new/0,
    in/2,
    in/3,
    out/1,
    all/1,
    clear/1
]).

-record(wf_action_queue,{eager,normal,defer}).

-type action_queue()    :: #wf_action_queue{}.
-type out_reply()       :: {error, empty} | {ok, actions(), action_queue()}.

-export_type([action_queue/0]).

-spec new() -> action_queue().
new() ->
    blank_state().
   
-spec blank_state() -> action_queue().
blank_state() ->
    #wf_action_queue{eager=queue:new(), normal=queue:new(), defer=queue:new()}.

-spec in(Val :: actions(), action_queue()) -> action_queue().
in(Val, Q) ->
    in(normal, Val, Q).

-spec in(Pri :: wire_priority(), Val :: actions(), action_queue()) -> action_queue().
in(eager, Val, AQ = #wf_action_queue{eager=Queue}) ->
    NewQueue = queue:in(Val,Queue),
    AQ#wf_action_queue{eager=NewQueue};

in(normal, Val, AQ = #wf_action_queue{normal=Queue}) ->
    NewQueue = queue:in(Val,Queue),
    AQ#wf_action_queue{normal=NewQueue};

in(defer, Val, AQ = #wf_action_queue{defer=Queue}) ->
    NewQueue = queue:in(Val,Queue),
    AQ#wf_action_queue{defer=NewQueue}.

-spec out(action_queue()) -> out_reply().
out(AQ = #wf_action_queue{}) ->
    QueueList = [
        AQ#wf_action_queue.eager,
        AQ#wf_action_queue.normal,
        AQ#wf_action_queue.defer
    ],
    case get_next(QueueList) of
        empty ->
            {error, empty};
        {ok, Value, [NewEager, NewNormal, NewDefer]} ->
            NewAQ = AQ#wf_action_queue{
                eager=NewEager,
                normal=NewNormal,
                defer=NewDefer
            },
            {ok, Value, NewAQ}
    end.

-spec all(action_queue()) -> [actions()].
all(AQ = #wf_action_queue{}) ->
    queue:to_list(AQ#wf_action_queue.eager)
        ++ queue:to_list(AQ#wf_action_queue.normal)
        ++ queue:to_list(AQ#wf_action_queue.defer).

-spec clear(action_queue()) -> action_queue().
clear(_) ->
    blank_state().

%% PRIVATE

get_next(Qs) ->
    get_next([], Qs).

get_next(_PassedQs, []) ->
    empty;
get_next(PassedQs, [CurQ | RestQ]) ->
    case queue:is_empty(CurQ) of
        true ->
            get_next([CurQ | PassedQs], RestQ);
        false ->
            {{value, Item}, NewQ} = queue:out(CurQ),
            {ok, Item, lists:reverse(PassedQs) ++ [NewQ | RestQ]}
    end.
