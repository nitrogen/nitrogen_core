%% vim: ts=4 sw=4 et
-module(element_sync_panel).
-include("wf.hrl").
-export([
    reflect/0,
    render_element/1,
    refresh/1,
    refresh/2,
    event/1,
    update/3,
    run_fun/1
]).

-define(STATE_KEY, sync_panel_body_trigger).
-spec reflect() -> [atom()].
reflect() -> record_info(fields, sync_panel).



-spec render_element(#sync_panel{}) -> #panel{}.
render_element(#sync_panel{body_fun=Fun}) 
        when not(is_function(Fun)) 
            andalso not(is_tuple(Fun) andalso (tuple_size(Fun)==2 orelse tuple_size(Fun)==3)) ->
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
    Panel#panel{body=run_fun(BodyFun)}.
   
run_fun(Fun) when is_function(Fun) ->
    Fun();
run_fun({M,F}) ->
    M:F();
run_fun({M,F,A}) ->
    erlang:apply(M,F,A).

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
            Body = run_fun(BodyFun),%BodyFun(),
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
                    Rendered = run_fun(BodyFun), %BodyFun(),
                    wf:update(Targetid, Rendered),
                    wf:flush();
                false ->
                    do_nothing
            end;
        not_alive ->
            exit(normal);
        {'JOIN', _} ->
            do_nothing;
        {'LEAVE', _} ->
            do_nothing
%%        %% Sending Body to all clients?
%%        %% Let's disable for now. Needs more thought.
%%        {body, T, Body} ->
%%            case lists:member(T, Triggers) of
%%                true ->
%%                    wf:update(Targetid, Body);
%%                false ->
%%                    do_nothing
%%            end
    after 20000 ->
        %% The comet process will continue to live for as long as the comet pool exists. 
        {ok, TRef} = timer:send_after(20000, self(), not_alive),
        wf:wire(#event{delegate=?MODULE, postback={still_alive, TRef}}),
        wf:flush()
    end,
    ?MODULE:update(Targetid, Triggers, BodyFun).

event({still_alive, TRef}) ->
    timer:cancel(TRef).
