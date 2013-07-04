%% vim: ts=4 sw=4 et
-module(wf_action_queue).
-include_lib("nitrogen_core/include/wf.hrl").
-behavior(gen_server).

-export([
    new/0,
    in/2,
    in/3,
    out/1,
    all/1,
    clear/1
]).

-export([
    start_link/0,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state,{eager,normal,defer}).
-record(wf_action_queue,{pid}).

%% This is a basic 3-queue priority queue.
%% At the initialization of each nitrogen request, priority_queue server will
%% be spawned for the request and linked to the nitrogen request process.
%%
%% start() or start_link() instantiate the server and return a "Queue" object.
%% This queue can be referred to 
%%

new() ->
    {ok, AQ} = start_link(),
    AQ.

start_link() ->
    {ok,Pid} = gen_server:start_link(?MODULE,self(),[]),
    {ok,#wf_action_queue{pid=Pid}}.

stop(#wf_action_queue{pid=Pid}) ->
    gen_server:call(die, Pid).

in(Val, Q) ->
    in(Val, normal, Q).

in(Pri, Val, #wf_action_queue{pid=Pid}) when Pri==normal orelse Pri==defer orelse Pri==eager ->
    gen_server:call(Pid, {in, Val, Pri}).

out(#wf_action_queue{pid=Pid}) ->
    gen_server:call(Pid, out).

all(#wf_action_queue{pid=Pid}) ->
    gen_server:call(Pid, all).

clear(#wf_action_queue{pid=Pid}) ->
    gen_server:call(Pid, clear).

%% gen_server functions

blank_state() ->
    #state{eager=queue:new(), normal=queue:new(), defer=queue:new()}.

init(_CallingPid) ->
    %% link and trap exits?
    {ok, blank_state()}.

terminate(_Reason,_State) ->
    ok.

handle_call({in,Value,eager},_Pid,State) ->
    NewQueue = queue:in(Value,State#state.eager),
    {reply, ok, State#state{eager=NewQueue}};
handle_call({in,Value,normal},_Pid,State) ->
    NewQueue = queue:in(Value,State#state.normal),
    {reply, ok, State#state{normal=NewQueue}};
handle_call({in,Value,defer},_Pid,State) ->
    NewQueue = queue:in(Value,State#state.defer),
    {reply, ok, State#state{defer=NewQueue}};
handle_call(out,_Pid,State) ->
    {Reply, [NewEager, NewNormal, NewDefer]} = 
        get_next({error,empty}, [
                State#state.eager,
                State#state.normal,
                State#state.defer]),
    NewState = State#state{
        eager=NewEager,
        normal=NewNormal,
        defer=NewDefer
    },
    {reply, Reply, NewState};
handle_call(clear, _Pid, _State) ->
    {reply, ok, blank_state()};
handle_call(all, _Pid, State) ->
    AllActions = queue:to_list(State#state.eager) 
                 ++ queue:to_list(State#state.normal)
                 ++ queue:to_list(State#state.defer),
    {reply, AllActions, State};

handle_call(die,_Pid,State) ->
    {stop,cancelled,ok,State}.

handle_cast(_Request,State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% PRIVATE

get_next(DefaultReply, Qs) ->
    get_next(DefaultReply, [], Qs).

get_next(DefaultReply, PassedQs, []) ->
    {DefaultReply, lists:reverse(PassedQs)};
get_next(DefaultReply, PassedQs, [CurQ | RestQ]) ->
    case queue:is_empty(CurQ) of
        true ->
            get_next(DefaultReply, [CurQ | PassedQs], RestQ);
        false ->
            {{value, Item}, NewQ} = queue:out(CurQ),
            Reply = {ok, Item},
            {Reply, lists:reverse(PassedQs) ++ [NewQ | RestQ]}
    end.
