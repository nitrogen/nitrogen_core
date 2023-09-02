%% vim: ft=erlang ts=4 sw=4 sts=4 et

-module(wf_fastmap).
-export([
    build/1,
    get/0,
    get/1,
    set/2,
    set/1,
    init/0,
    bench/0
]).
-compile({no_auto_import, [get/1, get/0]}).
-include("wf.hrl").
-include_lib("syntax_tools/include/merl.hrl").

%% This works like mochiglobal and persistent_term.  For the use-case this is
%% optimized for (very small terms for keys and values), it's between a 2x and
%% 10x speedup over persistent_term.
%%
%% It works like mochiglobal in that it compiles a lookup module (called
%% wf_fastmap_worker).
%%
%% This is not recommended to be used with large values (like 1MB+ binaries).
%% In that situation, use persistent_term, which will be *much* faster.
%% 
%% Also, like mochiglobal, this is *very* slow at writes. This is designed for
%% something that might be written to when the Erlang VM starts, and then very
%% rarely after that (like during a release upgrade, or if a config needs to
%% change).
%
%% The produced module looks something like this:
%%
%% -module(wf_fastmap_worker).
%% -compile(export_all).
%%
%% get(some_key) -> Val;
%% get(some_other_key) -> Val;
%% ...
%% get(_) -> error().
%% ...

-define(PTERM_FASTMAP_CONFIG, wf_fastmap_config).

store_config(Map) when is_map(Map) ->
    wf_utils:pterm(?PTERM_FASTMAP_CONFIG, Map).

get_config() ->
    wf_utils:pterm(?PTERM_FASTMAP_CONFIG).

init() ->
    Config = case get_config() of
        undefined ->
            default_config();
        Cfg ->
            Cfg
    end,
    build(Config).

default_config() ->
    %% Currently, the only defaults are the grid_system defaults, however,
    %% if new defaults are defined, the Configs can be combined (this is why
    %% we're not just calling wf_grid_system:init/0).  Doing it the way we're
    %% currently doing it will only require then setting the value once.
    Config = wf_grid_system:initial_config(),
    Config.

set(KV) when is_list(KV) ->
    NewMap = maps:from_list(KV),
    set(NewMap);
set(NewMap) when is_map(NewMap) ->
    CurrentMap = get(),
    FinalMap = maps:merge(CurrentMap, NewMap),
    build(FinalMap).

get() ->
    wf_fastmap_worker:get().

get(Key) ->
    wf_fastmap_worker:get(Key).

set(Key, Val) ->
    wf_fastmap_worker:set(Key, Val).


build(Map) when is_map(Map) ->
    store_config(Map),
    BuildMsg = "Building wf_fastmap_worker with this config:",
    TooMuch = "(If you see this more than a few times per day,\n you're probably updating this too much)",
	msg(wf:f("~s~n~p~n~n~s", [BuildMsg,Map, TooMuch])),
    build_inner(maps:to_list(Map)).

build_inner(PL) ->
	Modtext = build_module(PL),
	Forms = try merl:quote(Modtext)
            catch E:T:S ->
                msg(wf:f("Failed to parse wf_fastmap_worker module with the text: ~n~ts",[Modtext])),
                throw({E, T, S})
            end,

	Res = merl:compile_and_load(Forms),
    %logger:info("Building Module:~n~ts~n",[Modtext]),
	case Res of 
		{ok, _} -> ok;
		error ->
			msg(wf:f("Unable to build wf_fastmap_worker module with the text:~n~ts",[Modtext])),
			error
	end.

build_module(PL) ->
	lists:flatten([
		build_header(),
		build_exports(PL)
	]).

build_header() ->
	"-module(wf_fastmap_worker).\n"
    "-compile({no_auto_import, [get/1, get/0]}).\n"
	"-export([get/1, get/0, set/2]).\n\n".

build_exports(PL) ->
    [
        build_keys_0(PL),
        build_get_1(PL),
        build_get_0(),
        build_set_2()
    ].

build_get_1(PL) ->
    [
        [build_clause_get_1(KV) || KV <- PL],
        build_clause_get_1_catchall()
    ].

build_clause_get_1({Key, Value}) ->
    wf:f("get(~p) -> ~p;~n", [Key, Value]).

build_clause_get_1_catchall() ->
    "get(X) -> error({wf_fastmap_key_not_found, X}).\n\n".

build_keys_0(PL) ->
    Keys = proplists:get_keys(PL),
    wf:f("keys() -> ~p.~n~n",[Keys]).

build_get_0() ->
    "get() -> maps:from_list([{K, get(K)} || K <- keys()]).\n\n".

build_set_2() ->
    "set(Key, Val) ->
        Map = get(),
        Map2 = maps:put(Key, Val, Map),
        wf_fastmap:build(Map2).\n\n".

msg(Msg) ->
	logger:notice("Nitrogen Global FastMap: ~ts~n",[Msg]).

-define(TESTKEY(K), {nitrogen, K}).

bench() ->
    QuickMap = #{a=>b, b=>d, c=>f,
                 d=>crypto:strong_rand_bytes(100)},

    {BTime1, _} = timer:tc(fun() ->
        maps:map(fun(K, V) ->
            persistent_term:put(?TESTKEY(K), V)
        end, QuickMap)
    end),

    {BTime2, _} = timer:tc(fun() ->
        build(QuickMap)
    end),
    
    JustKeys = maps:keys(QuickMap),

    Keys = lists:map(fun(X) ->
        lists:nth(X rem 3 + 1, JustKeys)
    end, lists:seq(1, 1000000)),

    {Time1, _Res1} = timer:tc(fun() ->
        [persistent_term:get(?TESTKEY(K)) || K <- Keys]
    end),

    {Time2, _Res2} = timer:tc(fun() ->
        [wf_fastmap:get(K) || K <- Keys]
    end),
    
    {results, #{
        pterm_time=>Time1,
        fastmap_time=>Time2,
        pterm_store=>BTime1,
        fastmap_store=>BTime2}
    }.
