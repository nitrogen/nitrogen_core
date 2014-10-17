%% vim: ts=4 sw=4 et
-module(wf_test).
-include("wf.hrl").
-export([
    start_all/1,

    start/1,
    start_other/2,
    pass/1,
    fail/2,

    test_manual/2,
    test_manual/3,
    test_manual/4,

    test_auto/2,
    test_auto/3,
    test_auto/4,
    
    test/5,

    test_js/2,
    test_js/4,
    test_js/5,

    test_event/1,
    event/1,
    api_event/3
]).

start_all(App) ->
    timer:sleep(3000),
    {ok, Browsers} = application:get_env(App, test_browsers),
    {ok, Tests} = application:get_env(App, tests),

    lists:foreach(fun(Browser) ->
        error_logger:info_msg("Starting tests with ~s",[Browser]),
        Pid = wf_test_srv:start(Browser, Tests),
        error_logger:info_msg("Test Server Pid: ~p:",[Pid]),
        erlang:monitor(process, Pid),
        receive
            {'DOWN', _, process, Pid, _ } ->
                error_logger:info_msg("Finished tests with ~s~n~n",[Browser])
        end
    end, Browsers),
    init:stop().

start(TestFun) ->
    {ok, Pid} = wf:comet(fun() ->
        erlang:put(wf_test_passed, 0),
        erlang:put(wf_test_failed, 0),
        TestFun(),
        summarize_and_continue()
    end),
    wf:state(test_comet_pid, Pid).

start_other(OtherModule, TestFun) ->
    wf_context:page_module(OtherModule),
    start(TestFun),
    OtherModule:main().

summarize_and_continue() ->
    Passed = erlang:get(wf_test_passed),
    Failed = erlang:get(wf_test_failed),
    Total = Passed + Failed,
    io:format("Module ~p (~p of ~p tests passed)~n", [wf:page_module(), Passed, Total]),
    case wf_test_srv:next_test_path() of
        done -> wf:wire(#alert{text="All Tests Completed"});
        Next -> wf:redirect(Next)
    end.

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


test(AutoPostback, Name, Setup, _Assert=undefined, Timeout) ->
    test(AutoPostback, Name, Setup, fun() -> true end, Timeout);
test(AutoPostback, Name, _Setup=undefined, Assertion, Timeout) ->
    test(AutoPostback, Name, fun() -> ok end, Assertion, Timeout);
test(AutoPostback, Name, Setup, Assertion, Timeout) when is_function(Setup, 0), is_function(Assertion, 0) ->
    try
        wf:session({assertion, Name}, Assertion),
        Setup(),
        ?WF_IF(AutoPostback, wf:wire(#event{delegate=?MODULE, postback=Name})),
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

-define(wf_assert(Name, Assertion, Expectation),
    try Assertion of
        Expectation -> pass(Name);
        Other -> fail(Name, [{expected, Expectation}, {returned, Other}])
    catch
        Class:Error ->
            fail(Name, {Class, Error, erlang:get_stacktrace()})
    end).

test_event(Name) ->
    Assert = wf:session({assertion, Name}),
    ?wf_assert(Name, Assert(), true).

event(Name) ->
    test_event(Name).


test_js(Name, {Setup, JS, Assertion}) ->
    test_js(Name, Setup, JS, Assertion).

test_js(Name, Setup, JS, Assertion) -> 
    test_js(Name, Setup, JS, Assertion, 2000).

test_js(Name, _Setup=undefined, JS, Assertion, Timeout) ->
    test_js(Name, fun() -> ok end, JS, Assertion, Timeout);
test_js(Name, Setup, JS, _Assertion=undefined, Timeout) ->
    test_js(Name, Setup, JS, fun(_) -> true end, Timeout);
test_js(Name, Setup, JS, Assertion, Timeout) ->
    try
        wf:wire(#api{name=Name, tag=Assertion, delegate=?MODULE}),
        Setup(),
        wf:wire(wf:f("var f=function(){ ~s }; page.~p(f())", [JS, Name])),
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

api_event(Name, Assertion, Args) ->
    ?wf_assert(Name, Assertion(Args), true).

internal_pass(Name) ->
    increment_pass(),
    io:format("...Passed (~p)~n", [Name]).

internal_fail(Name, Reason) ->
    increment_fail(),
    io:format("...FAILED (~p).~n Reason: ~p~n", [Name, Reason]).
   
increment_pass() ->
    wf_test_srv:passed(1),
    N = erlang:get(wf_test_passed),
    erlang:put(wf_test_passed, N+1).

increment_fail() ->
    wf_test_srv:failed(1),
    N = erlang:get(wf_test_failed),
    erlang:put(wf_test_failed, N+1).


