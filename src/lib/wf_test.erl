-module(wf_test).
-include("wf.hrl").
-export([
	start/1,
	pass/1,
	fail/2
]).

start(TestFun) ->
	{ok, Pid} = wf:comet(TestFun),
	wf:state(test_comet_pid, Pid).
	
pass(Name) ->
	Pid = wf:state(test_comet_pid),
	Pid ! Name.

fail(Name, Reason) ->
	Pid = wf:state(test_comet_pid),
	Pid ! {fail, Name, Reason}.



