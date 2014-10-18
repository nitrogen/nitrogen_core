-ifndef(wf_test).
-define(wf_test, ok).

-define(wf_test_manual(Name, SetupAssertion),
	wf_test:test_manual(Name, SetupAssertion)).
-define(wf_test_manual(Name, Setup, Assertion), 
	wf_test:test_manual(Name, Setup, Assertion)).
-define(wf_test_manual(Name, Setup, Assertion, Options), 
	wf_test:test_manual(Name, Setup, Assertion, Options)).

-define(wf_test_auto(Name, SetupAssertion),
	wf_test:test_auto(Name, SetupAssertion)).
-define(wf_test_auto(Name, Setup, Assertion), 
	wf_test:test_auto(Name, Setup, Assertion)).
-define(wf_test_auto(Name, Setup, Assertion, Options),
	wf_test:test_auto(Name, Setup, Assertion, Options)).

-define(wf_test_event(Name),
	wf_test:event(Name)).

-define(wf_test_js(Name, SetupJSAssertion),
	wf_test:test_js(Name, SetupJSAssertion)).
-define(wf_test_js(Name, Setup, JS, Assertion),
	wf_test:test_js(Name, Setup, JS, Assertion)).
-define(wf_test_js(Name, Setup, JS, Asertion, Options),
	wf_test:test_js(Name, Setup, JS, Assertion, Options)).

-endif.
