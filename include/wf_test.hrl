-ifndef(wf_test).
-define(wf_test, ok).

-define(wf_test_manual(Name, SetupAssertion),
	wf_test:test_manual(Name, SetupAssertion)).
-define(wf_test_manual(Name, Setup, Assertion), 
	wf_test:test_manual(Name, Setup, Assertion)).
-define(wf_test_manual(Name, Setup, Assertion, Timeout), 
	wf_test:test_manual(Name, Setup, Assertion, Timeout)).

-define(wf_test_auto(Name, SetupAssertion),
	wf_test:test_auto(Name, SetupAssertion)).
-define(wf_test_auto(Name, Setup, Assertion), 
	wf_test:test_auto(Name, Setup, Assertion)).
-define(wf_test_auto(Name, Setup, Assertion, Timeout),
	wf_test:test_auto(Name, Setup, Assertion, Timeout)).

-define(wf_test_event(Name),
	wf_test:event(Name)).
-endif.
