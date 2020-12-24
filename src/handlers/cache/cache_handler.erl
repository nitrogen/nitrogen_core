% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (cache_handler).
-include("wf.hrl").
-export ([
    get_cached/3,
    set_cached/3,
    clear/1,
    clear_all/0
]).

-callback init(         Config :: term(),
                        State :: term()) -> {ok, State :: term() }.
-callback finish(       Config :: term(), 
                        State :: term()) -> {ok, State :: term() }.
-callback get_cached(   Key :: term(),
                        Function :: fun(),
                        TTL :: infinity | integer(),
                        Config :: term(),
                        State :: term()) -> {ok, Value :: any(), State :: term()}.
-callback set_cached(   Key :: term(),
                        Value :: term(),
                        TTL :: infinity | integer(),
                        Config :: term(),
                        State :: term()) -> {ok, State :: term()}.
-callback clear(        Key :: term(),
                        Config :: term(),
                        State :: term()) -> {ok, State :: term()}.
-callback clear_all(    Config :: term(),
                        State :: term()) -> {ok, State :: term()}.

% @doc get_cached(Key, Function, TTL, State) -> {ok, Value, NewState}
% Return the cache value associated with Key. If it is not found,
% then run the Function, store the resulting value in cache under
% Key, and return the value.
-spec get_cached(Key :: any(),
                 Function :: function(),
                 TTL :: integer() | undefined | infinity) -> {ok, term()}.
get_cached(Key, Function, TTL) ->  
    ContextedFun = fun() ->
        %% capture existing caching state before evaluating
        Caching = wf_context:caching(),
    
        %% let us know we're caching
        wf_context:caching(true),

        %% Process the caching function
        Result = Function(),

        %% Restore the caching status to its previous state (and if the state is unchanged, save some cycles by skipping it
        ?WF_IF(Caching, wf_context:caching(Caching)),

        %% Return the result
        Result
    end,
    {ok, _Value} = wf_handler:call(cache_handler, get_cached, [Key, ContextedFun, TTL]).

% @doc set_cached(Key, Value, TTL, State) -> {ok, NewState}
% Set the cached value directly without checking if it currently exists. If a
% value already exists for that key, it is replaced.
set_cached(Key, Value, TTL ) ->
    ok = wf_handler:call(cache_handler, set_cached, [Key, Value, TTL]).

% @doc Remove a value from cache.
-spec clear(Key :: any()) -> ok.
clear(Key) ->	
    ok = wf_handler:call(cache_handler, clear, [Key]).

% @doc Clear all values from cache.
-spec clear_all() -> ok.
clear_all() -> 
    ok = wf_handler:call(cache_handler, clear_all).
