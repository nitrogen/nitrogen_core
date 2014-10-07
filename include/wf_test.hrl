-ifndef(wf_test).
-define(wf_test, ok).

-define(wf_test_pass(Name), io:format("...Passed (~p)~n", [Name])).
-define(wf_test_fail(Name, Reason), io:format("...FAILED (~p). Reason: ~p~n", [Name, Reason])).

-define(wf_test(Name, Setup, Assertion), ?wf_test(Name, Setup, Assertion, 1000)).
-define(wf_test(Name, Setup, Assertion, Timeout),
	fun() ->
		try
			wf:session({assertion, Name}, Assertion),
			Setup(),
			wf:flush(),
			receive
				Name ->
					?wf_test_pass(Name);
				{fail, Name, Reason} ->
					?wf_test_fail(Name, Reason)
			after
				Timeout ->
					?wf_test_fail(Name, {timeout, Timeout})
			end
		catch
			Class:Error ->
				?wf_test_fail(Name, {Class, Error, erlang:get_stacktrace()})
		end
	end()).

-define(wf_test_event(Name), 
	Assert = wf:session({assertion, Name}),
	try Assert() of
		true -> wf_test:pass(Name);
		Result -> wf_test:fail(Name, {assert_returned, Result})
	catch
		Class:Error ->
			wf_test:fail(Name, {Class, Error, erlang:get_stacktrace()})
	end).


-endif.
