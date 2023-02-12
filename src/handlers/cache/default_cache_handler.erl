% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2015-2020 Jesse Gumm
% See MIT-LICENSE for licensing information.
%
% This cache handler relies on nitro_cache
%

-module (default_cache_handler).
-behaviour (cache_handler).
-include("wf.hrl").
-export ([
    init/2, 
    finish/2,
    get_cached/5,
    set_cached/5,
    clear/3, 
    clear_all/2
]).

init(Config, State) ->
    CacheName = cache_name(Config),
    try maybe_add_cache(CacheName)
    catch error:undef ->
        wf:error("nitro_cache not found in the system. You should add it as a dependency to your rebar.config like this:~n    ~p~n", [{nitro_cache, {git, "https://github.com/nitrogen/nitro_cache", {branch, master}}}]),
        throw({nitro_cache_not_found, "See message above"})
    end,
    {ok, State}.

%% This is here in case the nitro_cache application is restarted.
maybe_add_cache(CacheName) ->
    case application:get_env(nitro_cache, initialized_nitrogen_caches) of
        undefined ->
            add_cache([], CacheName);
        {ok, Caches} ->
            case lists:member(CacheName, Caches) of
                true -> ok;
                false -> add_cache(Caches, CacheName)
            end
    end.

add_cache(Caches, CacheName) ->
    try nitro_cache:init(CacheName)
    catch error:badarg ->
        case nitro_cache:cache_exists(CacheName) of
            true -> ok;
            false -> throw({cannot_init_cache, CacheName})
        end
    end,
    application:set_env(nitro_cache, initialized_nitrogen_caches, [CacheName | Caches]).

finish(_Config, State) -> 
    {ok, State}.

-spec get_cached(term(), fun(), integer() | infinity, proplist(), any()) -> {ok, term(), any()}.
get_cached(Key, Function, TTL, Config, State)
        when is_function(Function, 0) -> 
    CacheName = cache_name(Config),
    Return = nitro_cache:get(CacheName, TTL, Key, Function),
    {ok, Return, State}.

set_cached(Key, Value, TTL, Config, State) ->
    CacheName = cache_name(Config),
    nitro_cache:set(CacheName, TTL, Key, Value),
    {ok, State}.

clear(Key, Config, State) -> 
    nitro_cache:flush(cache_name(Config), Key),
    {ok, State}.

clear_all(Config, State) -> 
    nitro_cache:flush(cache_name(Config)),
    {ok, State}.

cache_name(undefined) ->
    nitrogen;
cache_name(Config) ->
    proplists:get_value(cache_name, Config, nitrogen).
