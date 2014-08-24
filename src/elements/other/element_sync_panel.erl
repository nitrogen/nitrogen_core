%% vim: ts=4 sw=4 et
-module(element_sync_panel).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    refresh/1,
    refresh/2
]).

-define(STATE_KEY, sync_panel_body_trigger).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, sync_panel).

-spec render_element(#sync_panel{}) -> #panel{}.
render_element(#sync_panel{body_fun=Fun}) when not(is_function(Fun)) ->
    throw({sync_panel_element, {body_fun_not_a_function, Fun}});
render_element(E = #sync_panel{
        body_fun=BodyFun,
        triggers=Triggers,
        pool=Pool}) ->
        
    Targetid = wf:temp_id(),
    SyncPanel = E#sync_panel{id=Targetid},
    wf:comet_global(fun() -> update(Targetid, Triggers, BodyFun) end, Pool),
    register_target_body_fun(Targetid, BodyFun, Pool, Triggers),
    Panel = wf_utils:copy_fields(SyncPanel, #panel{}),
    Panel#panel{body=BodyFun()}.
   
-spec register_target_body_fun(Targetid :: id(), BodyFun :: fun(), Pool :: term(), [Trigger :: term()]) -> ok.
register_target_body_fun(_,_,_,[]) -> ok;
register_target_body_fun(Targetid, BodyFun, Pool, [Trigger|Triggers]) ->
    wf:state({?STATE_KEY, Pool, Trigger},{Targetid, BodyFun}),
    register_target_body_fun(Targetid, BodyFun, Pool, Triggers).

-spec get_target_body_fun(Pool :: term(), Trigger :: term()) -> {Targetid :: id(), BodyFun :: fun()} | undefined.
get_target_body_fun(Pool, Trigger) ->
    wf:state({?STATE_KEY, Pool, Trigger}).

-spec refresh(Trigger :: term()) -> ok.
refresh(Trigger) ->
    refresh(sync_panel, Trigger).
    
-spec refresh(Pool :: term(), Trigger :: term()) -> ok.
refresh(Pool, Trigger) ->
    case get_target_body_fun(Pool, Trigger) of
        undefined ->
            wf:send_global(Pool, {trigger, Trigger});
        {Targetid, BodyFun} ->
            Body = BodyFun(),
            wf:update(Targetid, Body),
            wf:send_global(Pool, {trigger, Trigger})
            %% Disabled sending generated body across.
            %% wf:send_global(Pool, {body, Trigger, Body})
    end.


%% This is the private cycle that receives message
update(Targetid, Triggers, BodyFun) ->
    receive 
        {trigger, T} ->
            case lists:member(T, Triggers) of
                true ->
                    Rendered = BodyFun(),
                    wf:update(Targetid, Rendered),
                    wf:flush();
                false ->
                    do_nothing
            end
%%        %% Sending Body to all clients?
%%        %% Let's disable for now. Needs more thought.
%%        {body, T, Body} ->
%%            case lists:member(T, Triggers) of
%%                true ->
%%                    wf:update(Targetid, Body);
%%                false ->
%%                    do_nothing
%%            end
    end,
    update(Targetid, Triggers, BodyFun).
