%% vim: ts=4 sw=4 et
-module(wf_test).
-include("wf.hrl").
-export([
    start/1,
    pass/1,
    fail/2,

    test_manual/2,
    test_manual/3,
    test_manual/4,

    test_auto/2,
    test_auto/3,
    test_auto/4,
    
    test/5,

    test_event/1,
    event/1
]).

start(TestFun) ->
    {ok, Pid} = wf:comet(fun() ->
        erlang:put(wf_test_passed, 0),
        erlang:put(wf_test_failed, 0),
        TestFun(),
        summary()
    end),
    wf:state(test_comet_pid, Pid).

summary() ->
    Passed = erlang:get(wf_test_passed),
    Failed = erlang:get(wf_test_failed),
    Total = Passed + Failed,
    io:format("Module ~p (~p of ~p tests passed)~n", [wf:page_module(), Passed, Total]).

pass(Name) ->
    Pid = wf:state(test_comet_pid),
    Pid ! Name.

fail(Name, Reason) ->
    Pid = wf:state(test_comet_pid),
    Pid ! {fail, Name, Reason}.

test_manual(Name, {Setup, Assertion}) ->
    test_manual(Name, Setup, Assertion).

test_manual(Name, Setup, Assertion) ->
    test_manual(Name, Setup, Assertion, 2000).

test_manual(Name, Setup, Assertion, Timeout) ->
    test(false, Name, Setup, Assertion, Timeout).

test_auto(Name, {Setup, Assertion}) ->
    test_auto(Name, Setup, Assertion).

test_auto(Name, Setup, Assertion) ->
    test_auto(Name, Setup, Assertion, 2000).

test_auto(Name, Setup, Assertion, Timeout) ->
    test(true, Name, Setup, Assertion, Timeout).

test(AutoPostback, Name, Setup, Assertion, Timeout) when is_function(Setup, 0), is_function(Assertion, 0) ->
    try
        wf:session({assertion, Name}, Assertion),
        Setup(),
        ?WF_IF(AutoPostback, wf:wire(#event{type=timer, delay=1, delegate=?MODULE, postback=Name})),
        wf:flush(),
        receive
            Name ->
                internal_pass(Name);
            {fail, Name, Reason} ->
                internal_fail(Name, Reason)
        after
            Timeout ->
                internal_fail(Name, {timeout, Timeout})
         end
    catch
        Class:Error ->
            internal_fail(Name, {Class, Error, erlang:get_stacktrace()})
    end.

test_event(Name) ->
    Assert = wf:session({assertion, Name}),
    try Assert() of
        true -> pass(Name);
        Result -> fail(Name, {assert_returned, Result})
    catch
        Class:Error ->
            fail(Name, {Class, Error, erlang:get_stacktrace()})
    end.

event(Name) ->
    test_event(Name).

internal_pass(Name) ->
    increment_pass(),
    io:format("...Passed (~p)~n", [Name]).

internal_fail(Name, Reason) ->
    increment_fail(),
    io:format("...FAILED (~p). Reason: ~p~n", [Name, Reason]).
   
increment_pass() ->
    N = erlang:get(wf_test_passed),
    erlang:put(wf_test_passed, N+1).

increment_fail() ->
    N = erlang:get(wf_test_failed),
    erlang:put(wf_test_failed, N+1).



