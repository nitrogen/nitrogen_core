-module(nitrogen_handler_srv).
-behaviour(gen_server).
-include("wf.hrl").

%% API
-export([start_link/0,
         get_handler/1,
         set_handler/1,
         set_handler/2,
         set_handler/3,
         update_state/2,
         build_handler_mapper/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-define(TABLE, nitrogen_handlers).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_state(HandlerName, HandlerState) ->
    gen_server:call(?SERVER, {set_state, HandlerName, HandlerState}).

get_handler(HandlerName) ->
    case ets:lookup(?TABLE, HandlerName) of
        [H = #handler_context{}] -> H;
        _ -> undefined
    end.

set_handler(HandlerModule) ->
    set_handler(HandlerModule, []).

set_handler(HandlerModule, Config) ->
    HandlerName = wf_handler:get_handler_name(HandlerModule),
    set_handler(HandlerName, HandlerModule, Config).

set_handler(HandlerName, HandlerModule, Config) ->
    ?PRINT({calling_start, HandlerName, HandlerModule, Config}),
    {ok, State} = maybe_call_start(HandlerModule, Config),
    gen_server:call(?SERVER, {register_handler, HandlerName, HandlerModule, Config, State}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    DefaultGlobalHandlers = wf_handler:default_global_handlers(),
    HandlerDefaults = wf_handler:handler_order(),
    GlobalHandlers = [{Name, wf_handler:handler_default(Name)} || Name <- HandlerDefaults,
                                                                  lists:member(Name, DefaultGlobalHandlers)],

    ok = init_ets(),
    add_global_handlers(GlobalHandlers),
    build_handler_mapper(),
    {ok, #state{}}.

handle_call({unregister_handler, HandlerName}, _From, State) ->
    Reply = remove_global_handler(HandlerName),
    build_handler_mapper(),
    {reply, Reply, State};
handle_call({register_handler, HandlerName, HandlerModule, HandlerConfig, HandlerState}, _From, State) ->
    Reply = add_global_handler(HandlerName, HandlerModule, HandlerConfig, HandlerState),
    build_handler_mapper(),
    {reply, Reply, State};
handle_call({set_state, HandlerName, HandlerState}, _From, State) ->
    Reply = internal_update_global_handler_state(HandlerName, HandlerState),
    build_handler_mapper(),
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_ets() ->
    ?TABLE = ets:new(?TABLE, [named_table, set, protected, {keypos, 2}, {read_concurrency, true}]),
    ok.

add_global_handlers([]) ->
    ok;
add_global_handlers([{HandlerName, Module} | Rest]) ->
    add_global_handler(HandlerName, Module, []),
    add_global_handlers(Rest).

add_global_handler(HandlerName, Module, Config) ->
    add_global_handler(HandlerName, Module, Config, []).


add_global_handler(HandlerName, Module, Config, State) ->
    Handler = #handler_context{
        name=HandlerName,
        module=Module,
        config=Config,
        state=State
    },
    ets:insert(?TABLE, Handler).

remove_global_handler(HandlerName) ->
    ets:delete(?TABLE, HandlerName).

internal_update_global_handler_state(HandlerName, State) ->
    case ets:lookup(?TABLE, HandlerName) of
        [H = #handler_context{}] ->
            NewHandler = H#handler_context{state=State},
            ets:insert(?TABLE, NewHandler),
            ok;
        _ ->
            logger:error("Warning: Attempted to update state for a global handler that doesn't exists.~nHandler Name: ~p~nState: ~p~n",[HandlerName, State]),
            error
    end.

maybe_call_start(HandlerModule, Config) ->
    case erlang:function_exported(HandlerModule, start, 1) of
        true ->
            %% Since this handler module has a "start" component to it, let's start it here.
            {ok, _State} = HandlerModule:start(Config);
        false ->
            %% If there is no start function, then nothing to call, and the state will just be an empty list
            {ok, []}
    end.

build_handler_mapper() ->
    Handlers = ets:tab2list(?TABLE),
    wf_handler_mapper_builder:build(Handlers).
