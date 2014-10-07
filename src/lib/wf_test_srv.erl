%% vim: ts=4 sw=4 et
-module(wf_test_srv).
-include("wf.hrl").
-behaviour(gen_server).
-export([main/0]).

-define(TIMEOUT, 15000).

%% API
-export([
    start/1,
    start/2,
    passed/1,
    failed/1,
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

-record(state, {trigger, test_paths=[], current_test, passed=0, failed=0}).

main() ->
    Trigger = wf:to_integer(wf:q(id)),
    case is_trigger_valid(Trigger) of
        false ->
            "Invalid Test Launch Path";
        true ->
            FirstTestPath = next_test_path(),
            wf:redirect(FirstTestPath)
    end.

start(TestPaths) ->
    start(undefined, TestPaths).

start(BrowserExec, TestPaths) ->
    Trigger = crypto:rand_uniform(1, 1000000000),
    LaunchUrl = wf:f("http://127.0.0.1:8000/wf_test_srv?id=~p", [Trigger]),
    error_logger:info_msg("Starting Nitrogen Test Server...~nOpen your browser to:~n        ~s~n", [LaunchUrl]),
    gen_server:start({local, ?MODULE}, ?MODULE, [Trigger, TestPaths], []),
    maybe_launch_browser(BrowserExec, LaunchUrl).

maybe_launch_browser(undefined, _) ->
    ok;
maybe_launch_browser(BrowserExec, LaunchUrl) ->
    os:cmd(BrowserExec ++ " " ++ LaunchUrl ++ " &").

is_trigger_valid(Trigger) ->
    try gen_server:call(?MODULE, {is_trigger_valid, Trigger})
    catch exit:{noproc,_} -> false
    end.

passed(Num) ->
    gen_server:cast(?MODULE, {passed, Num}).

failed(Num) ->
    gen_server:cast(?MODULE, {failed, Num}).

next_test_path() ->
    case gen_server:call(?MODULE, next_test_path) of
        {ok, Next} ->
            Next;
        undefined ->
            print_summary_and_close(),
            done
    end.

print_summary_and_close() ->
    {ok, Summary} = get_summary(),
    Passed = proplists:get_value(passed, Summary),
    Failed = proplists:get_value(failed, Summary),
    MsgFun = ?WF_IF(Failed > 0, error_msg, info_msg),
    error_logger:MsgFun("Tests Finished. ~p Passed. ~p Failed.", [Passed, Failed]),
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
handle_call(next_test_path, _From, State=#state{test_paths=[Next | Rest]}) ->
    Reply = {ok, Next},
    {reply, Reply, State#state{test_paths=Rest, current_test=Next}, ?TIMEOUT};
handle_call(next_test_path, _From, State=#state{test_paths=[]}) ->
    Reply = undefined,
    {reply, Reply, State#state{current_test=undefined}, ?TIMEOUT};
handle_call(summary, _From, State=#state{passed=Passed, failed=Failed}) ->
    Reply = {ok, [{passed, Passed}, {failed, Failed}]},
    {reply, Reply, State, ?TIMEOUT}.

handle_cast({passed, Num}, State=#state{passed=Cur}) ->
    {noreply, State#state{passed=Cur+Num}, ?TIMEOUT};
handle_cast({failed, Num}, State=#state{failed=Cur}) ->
    {noreply, State#state{failed=Cur+Num}, ?TIMEOUT};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(timeout, State=#state{test_paths=TestPaths, current_test=Cur}) ->
    Remaining = length(TestPaths),
    error_logger:error_msg("Tests Timed Out. Test '~s' never finished. ~p remaining tests!", [Cur, Remaining]),
    {stop, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
