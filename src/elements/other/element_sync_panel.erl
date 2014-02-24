%% vim: ts=4 sw=4 et
-module(element_sync_panel).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    refresh/1,
    refresh/2
]).

-define(STATE_KEY, sync_panel_render).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, sync_panel).

-spec render_element(#sync_panel{}) -> #panel{}.
render_element(#sync_panel{render_fun=Fun}) when not(is_function(Fun)) ->
    throw({sync_panel_element, render_fun_is_not_function});
render_element(E = #sync_panel{
        render_fun=RenderFun,
        triggers=Triggers,
        pool=Pool}) ->
        
    Targetid = wf:temp_id(),
    SyncPanel = E#sync_panel{id=Targetid},
    wf:comet_global(fun() -> update(Targetid, Triggers, RenderFun) end, Pool),
    register_target_render_fun(Targetid, RenderFun, Pool, Triggers),
    Panel = wf_utils:copy_fields(SyncPanel, #panel{}),
    Panel#panel{body=RenderFun()}.
   
-spec register_target_render_fun(Targetid :: id(), RenderFun :: fun(), Pool :: term(), [Trigger :: term()]) -> ok.
register_target_render_fun(_,_,_,[]) -> ok;
register_target_render_fun(Targetid, RenderFun, Pool, [Trigger|Triggers]) ->
    wf:state({?STATE_KEY, Pool, Trigger},{Targetid, RenderFun}),
    register_target_render_fun(Targetid, RenderFun, Pool, Triggers).

-spec get_target_render_fun(Pool :: term(), Trigger :: term()) -> {Targetid :: id(), RenderFun :: fun()} | undefined.
get_target_render_fun(Pool, Trigger) ->
    wf:state({?STATE_KEY, Pool, Trigger}).

-spec refresh(Trigger :: term()) -> ok.
refresh(Trigger) ->
    refresh(sync_panel, Trigger).
    
-spec refresh(Pool :: term(), Trigger :: term()) -> ok.
refresh(Pool, Trigger) ->
    case get_target_render_fun(Pool, Trigger) of
        undefined ->
            wf:send_global(Pool, {trigger, Trigger});
        {Targetid, RenderFun} ->
            Rendered = RenderFun(),
            wf:update(Targetid, Rendered),
            wf:send_global(Pool, {trigger, Trigger})
            %wf:send_global(Pool, {body, Trigger, Rendered})
    end.


%% This is the private cycle that receives message
update(Targetid, Triggers, RenderFun) ->
    receive 
        {trigger, T} ->
            case lists:member(T, Triggers) of
                true ->
                    Rendered = RenderFun(),
                    wf:update(Targetid, Rendered);
                false ->
                    do_nothing
            end;
        {body, T, Body} ->
            case lists:member(T, Triggers) of
                true ->
                    wf:update(Targetid, Body);
                false ->
                    do_nothing
            end
    end,
    wf:flush(),
    update(Targetid, Triggers, RenderFun).
