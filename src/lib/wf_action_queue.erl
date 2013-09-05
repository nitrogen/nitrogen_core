%% vim: ts=4 sw=4 et
-module(wf_action_queue).
-include("wf.hrl").
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

-record(state,{eager,normal,defer,caller}).
-record(wf_action_queue,{
    pid     :: pid()
}).

-type action_queue() :: #wf_action_queue{}.

%% This is a basic 3-queue priority queue.
%% At the initialization of each nitrogen request, priority_queue server will
%% be spawned for the request and linked to the nitrogen request process.
%%
%% start() or start_link() instantiate the server and return a "Queue" object.
%% This queue can be referred to 
%%

-spec new() -> action_queue().
new() ->
    {ok, AQ} = start_link(),
    AQ.

-spec start_link() -> {ok, action_queue()}.       
start_link() ->
    Caller = self(),
    {ok,Pid} = gen_server:start_link(?MODULE,Caller,[]),
    {ok,#wf_action_queue{pid=Pid}}.

-spec stop(action_queue()) -> ok.
stop(#wf_action_queue{pid=Pid}) ->
    gen_server:call(die, Pid).

-spec in(Val :: actions(), Q :: action_queue()) -> ok.
in(Val, Q) ->
    in(normal, Val, Q).

-spec in(Pri :: wire_priority(), Val :: actions(), action_queue()) -> ok.
in(Pri, Val, #wf_action_queue{pid=Pid}) when Pri==normal orelse Pri==defer orelse Pri==eager ->
    gen_server:call(Pid, {in, Val, Pri}).

-spec out(action_queue()) -> {ok, actions()} | {error, empty}.
out(#wf_action_queue{pid=Pid}) ->
    gen_server:call(Pid, out).

-spec all(action_queue()) -> [actions()].
all(#wf_action_queue{pid=Pid}) ->
    gen_server:call(Pid, all).

-spec clear(action_queue()) -> ok.
clear(#wf_action_queue{pid=Pid}) ->
    gen_server:call(Pid, clear).

%% gen_server functions

-spec blank_state(Caller :: pid()) -> #state{}.
blank_state(Caller) ->
    #state{eager=queue:new(), normal=queue:new(), defer=queue:new(), caller=Caller}.

-spec init(Caller :: pid()) -> {ok, #state{}}.
init(Caller) ->
    process_flag(trap_exit, true),
    {ok, blank_state(Caller)}.

-spec terminate(any(), #state{}) -> ok.
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
handle_call(clear, _Pid, State) ->
    {reply, ok, blank_state(State#state.caller)};
handle_call(all, _Pid, State) ->
    AllActions = queue:to_list(State#state.eager) 
                 ++ queue:to_list(State#state.normal)
                 ++ queue:to_list(State#state.defer),
    {reply, AllActions, State};

handle_call(die,_Pid,State) ->
    {stop,cancelled,ok,State}.

handle_cast(_Request,State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    case State#state.caller=:=Pid of
        true ->
            {stop, normal, State};
        false ->
            {noreply, State}
    end;
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
