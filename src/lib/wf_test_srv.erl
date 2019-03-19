%% vim: ts=4 sw=4 et
-module(wf_test_srv).
-include("wf.hrl").
-behaviour(gen_server).
-export([main/0]).

-define(TIMEOUT, 60000).

%% API
-export([
    start/1,
    start/2,
    start/3,
    passed/1,
    failed/1,
    set_autoadvance/1,
    next_test_path/0,
    get_summary/0,
    stop/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {trigger, autoadvance=true, test_paths=[], current_test, passed=0, failed=0}).

main() ->
    Trigger = wf:to_integer(wf:q(id)),
    case is_trigger_valid(Trigger) of
        false ->
            "Invalid Test Launch Path";
        true ->
            FirstTestPath = next_test_path(),
            wf:redirect(FirstTestPath)
    end.

start(AppName) when is_atom(AppName) ->
    {ok, TestPaths} = application:get_env(AppName, tests),
    %% when we remove support for R15, we can change this to get_env/3
    Opts = case application:get_env(AppName, test_options) of
        undefined -> [];
        {ok, Op} -> Op
    end,
    start(undefined, TestPaths, Opts).

start(BrowserExec, TestPaths) ->
    start(BrowserExec, TestPaths, []).

start(BrowserExec, TestPaths, Opts) ->
    BaseUrl = case proplists:get_value(base_url, Opts) of
        undefined -> "http://127.0.0.1:8000";
        Base -> Base
    end,
    Trigger = ?WF_UNIQUE,
    LaunchUrl = wf:f(BaseUrl ++ "/wf_test_srv?id=~p", [Trigger]),
    wf_test:log("Starting Nitrogen Test Server...~nOpen your browser to:~n        ~s~n", [LaunchUrl]),
    {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [Trigger, TestPaths], []),
    maybe_launch_browser(BrowserExec, LaunchUrl),
    Pid.

maybe_launch_browser(undefined, _) ->
    ok;
maybe_launch_browser(BrowserExec, LaunchUrl) ->
    os:cmd(BrowserExec ++ " " ++ LaunchUrl ++ " &"),
    ok.

is_trigger_valid(Trigger) ->
    try gen_server:call(?MODULE, {is_trigger_valid, Trigger})
    catch exit:{noproc,_} -> false
    end.

passed(Num) ->
    gen_server:cast(?MODULE, {passed, Num}).

failed(Num) ->
    gen_server:cast(?MODULE, {failed, Num}).

set_autoadvance(TF) when is_boolean(TF) ->
    gen_server:cast(?MODULE, {set_autoadvance, TF}).

next_test_path() ->
    case gen_server:call(?MODULE, next_test_path) of
        {ok, Next} ->
            Next;
        autoadvance_disabled ->
            %% autoadvance is disabled
            autoadvance_disabled;
        undefined ->
            print_summary_and_close(),
            done
    end.

print_summary_and_close() ->
    {ok, Summary} = get_summary(),
    Passed = proplists:get_value(passed, Summary),
    Failed = proplists:get_value(failed, Summary),
    wf_test:log("SUMMARY: Tests Finished. ~p Passed. ~p Failed.", [Passed, Failed]),
    stop().

get_summary() ->
    gen_server:call(?MODULE, summary).

stop() ->
    gen_server:cast(?MODULE, stop).

init([Trigger, TestPaths]) ->
    {ok, #state{trigger=Trigger, test_paths=TestPaths}, ?TIMEOUT}.

handle_call({is_trigger_valid, ProvidedTrigger}, _From, State=#state{trigger=Trigger}) ->
    Reply = ProvidedTrigger == Trigger,
    {reply, Reply, State, ?TIMEOUT};
handle_call(next_test_path, _From, State=#state{autoadvance=false}) ->
    {reply, autoadvance_disabled, State};
handle_call(next_test_path, _From, State=#state{test_paths=[Next | Rest]}) ->
    Reply = {ok, Next},
    {reply, Reply, State#state{test_paths=Rest, current_test=Next}, ?TIMEOUT};
handle_call(next_test_path, _From, State=#state{test_paths=[]}) ->
    Reply = undefined,
    {reply, Reply, State#state{current_test=undefined}, ?TIMEOUT};
handle_call(summary, _From, State=#state{passed=Passed, failed=Failed}) ->
    Reply = {ok, [{passed, Passed}, {failed, Failed}]},
    {reply, Reply, State, ?TIMEOUT}.

handle_cast({set_autoadvance, TF}, State) ->
    {noreply, State#state{autoadvance=TF}};
handle_cast({passed, Num}, State=#state{passed=Cur}) ->
    {noreply, State#state{passed=Cur+Num}, ?TIMEOUT};
handle_cast({failed, Num}, State=#state{failed=Cur}) ->
    {noreply, State#state{failed=Cur+Num}, ?TIMEOUT};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, State=#state{test_paths=TestPaths, current_test=Cur}) ->
    Remaining = length(TestPaths),
    wf_test:log("ERROR: Tests Timed Out. Test '~s' never finished. ~p remaining tests!", [Cur, Remaining]),
    {stop, timeout, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
