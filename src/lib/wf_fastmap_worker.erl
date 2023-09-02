%% This module will be overridden by wf_fastmap on the fly, but on the first
%% call of each function will recompile itself before calling itself again with
%% the newly compiled and loaded module.
%% This will then work during a release upgrade that may reload this base code,
%% because in wf_fastmap, all calls that set values are also loaded into
%% persistent_term.

-module(wf_fastmap_worker).
-export([get/1, get/0, set/2]).

get() ->
    wf_fastmap:init(),
    ?MODULE:get().

get(X) ->
    wf_fastmap:init(),
    ?MODULE:get(X).

set(K,V) ->
    wf_fastmap:init(),
    ?MODULE:set(K, V).
