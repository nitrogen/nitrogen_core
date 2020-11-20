<!-- dash: Automated Testing | Guide |  -->


# Automated Testing with Nitrogen

## Overview 

  Nitrogen has a simple unit testing framework which runs on the server and in
  the browser, and can be used for testing the functionality of your
  application, ensuring that objects render properly in the browser, and that
  actions are wired in the browser appropriately.

  This means that currently, you need to be able to run it on a machine with a
  browser in it, or there is an option to initiate a test from an external
  browser.

### The testing system consists of three layers:

#### 1. Individual Tests

    Each individual test is initiated by a call to one of the `?wf_test` macros
    and is used to test some simple cause and effect.

    There are a few different ways to initiate a test:

 *  `?wf_test_auto(TestName, SetupFun, AssertionFun)` :: This is the most
      basic form of test. The test system will call `SetupFun()`, and what that
      completes, will automatically initiate a postback that will call
      `AssertionFun()`.  AssertionFun is expected to return `true`. Anything else
      will result in the test failing.

      An example that tests that a value was properly set in the browser might be:

```erlang
      tests() ->
        SetupFun = fun() -> wf:set(my_textbox, "Value") end,
        AssertionFun = fun() -> wf:q(my_textbox) == "Value" end,
        ?wf_test_manual(set_textbox, SetupFun, AssertionFun).

```

      If something is broken for some reason, (most common would be a typo in
      the name of the element), then `wf:q(my_textbox)` would not return the
      expected value, and the test would fail.

 *  `?wf_test_manual(TestName, SetupFun, AssertionFun)` :: This is very
      similar to the `?wf_test_auto` test, except that the postback is not
      automatically executed.  Instead, you need to tell the test how to initiate
      the postback.  The easiest way would be with the `#event{}` or `#click{}`
      actions.

      An example of this might be to ensure that the expected postback is assigned to a button:

```erlang
      tests() ->
        SetupFun = fun() -> wf:wire(my_button, #click{}) end,
        AssertionFun = undefined,
        ?wf_test_manual(button_works, SetupFun, AssertionFun)

```

      And in the `event/1` function that `my_button` posts back to, you would add:

```erlang
        ?wf_test_event(button_works)

```

      Note that this is safe to include in your normal posbacks, since if the
      test is not running, this call will not do anything.

 *  `?wf_test_js(TestName, SetupFun, JS, AssertionFun)` :: This test method
      allows you to collect arbitrary javascript properties (like if you want to
      verify that an element on the page has a certain height, or maybe that some
      global javascript variable has been set to a specific value).  The
      arguments are **very** similar, except for a few changes.

 *  `SetupFun` is exactly the same as in `?wf_test_manual` in that you do
        not need to execute a postback in it.
 *  `JS` is a Javascript string that returns a value (the last call should
        be actuall have a `return` statement, as it's executed inside a javascript
        function). This value will be passed to the `AssertionFun` function.
 *  `AssertionFun` is expected to be a function of arity 1, with the only
        argument being a list containing the return value(s) from `JS`.

      Here's an example to illustrate the usage.  Here we will add a `#panel`
      (which is just an HTML `div`), then verify that its width is greater than
      zero:

```erlang
        SetupFun = fun() -> wf:insert_top(my_wrapper, #panel{id=my_new_panel, text="Hi Mom!"} end,
        JS = "return objs('my_wrapper').width()",
        AssertFun = fun([Width]) -> is_integer(Width) andalso Width > 0 end,
        ?wf_test_js(element_added, SetupFun, JS, AssertFun).

```

##### Options

     There is an optional last argument that can be passed to each of these
     macros: A proplist containing options for `delay` and `timeout`

 *  `delay` (`integer()`) :: The number of milliseconds to wait before
       executing the `JS` term (if using `?wf_test_js`) or to wait before
       executing the postback (if using `?wf_test_auto` or `?wf_test_manual`).
       This would be used to allow a test to /process/ before executing (say, an
       image may need to be downloaded, or some jquery-ui scripts will need to be
       run, or we need to wait for another postback to happen before getting the
       data).  This gives your script time to run before checking the value.

 *  `timeout` (`integer()`) :: The number of milliseconds the testing system
       is willing to wait for the test to complete before timing out.  This might
       be tripped if something is taking much longer than expected, or if
       javascript crashes and stops processing postbacks, or something else
       crashes.  It will just mean that the `?wf_test_event` macro was ultimately
       never called (which is done automatically if you're using `?wf_test_auto`
       or `?wf_test_js`).

##### Other Ways to call the Test Macros

     There are actually several ways to call each of the `?wf_test` macros:

 *  `?wf_test_auto(TestName, SetupFun, AssertionFun)`
 *  `?wf_test_auto(TestName, SetupFun, AssertionFun, Options)`
 *  `?wf_test_auto(TestName, {SetupFun, AssertionFun})`
 *  `?wf_test_auto(TestName, {SetupFun, AssertionFun, Options})`

 *  `?wf_test_manual(TestName, SetupFun, AssertionFun)`
 *  `?wf_test_manual(TestName, SetupFun, AssertionFun, Options)`
 *  `?wf_test_manual(TestName, {SetupFun, AssertionFun})`
 *  `?wf_test_manual(TestName, {SetupFun, AssertionFun, Options})`

 *  `?wf_test_js(TestName, SetupFun, JS, AssertionFun)`
 *  `?wf_test_js(TestName, SetupFun, JS, AssertionFun, Options)`
 *  `?wf_test_js(TestName, {SetupFun, JS, AssertionFun})`
 *  `?wf_test_js(TestName, {SetupFun, JS, AssertionFun, Options})`
     
     The reasoning for the alternative options of having the second argument be
     a tuple will be more obvious in the next section.

     Additionally, `SetupFun` and `AssertionFun` can be the atom `undefined`.
     If this is the case, `SetupFun` will do no setup (the equivilant of =fun()
     -> ok end`), and `AssertionFun` if set to `undefined= will simply return
     `true` (which you may do if all you wish to do is verify that a postback
     happened, without caring about the contents of the postback or the state
     of the application at the time of the postback).

#### 2. Test Pages
  
    A test page allows for a collection of tests to be run, and the results of
    each test gets aggregated on a per-page basis.

    Starting a page test consists of calling `wf_test:start(TestFuns)`, where
    `TestFun` is a function that consists of a series of `?wf_test_X()` calls
    along with any additional setup or teardown for the tests.

##### A page dedicated to tests:

     A simple example would be to call `wf_test:start/1` in your page module's
     `main()` function.
 
```erlang
       main() -> 
         wf_test:start(fun tests/0),
         #template{file="mytemplate.html"}.
 
       ...
 
       event(my_test_manual) ->
         ?wf_test_event(my_test_manual).
       
       tests() ->
         ?wf_test_auto(my_test, my_test()),
         ?wf_test_manual(my_other_test, my_manual_test()).
 
       my_test() ->
         {
           fun() -> wf:set(textbox, "NewVal") end,
           fun() -> wf:q(textbox) == "NewVal"
         }.
 
       my_test_manual() ->
         {
           fun() ->
             wf:insert_after(textbox, #button{postback=my_test_manual, id=mybutton}),
             wf:wire(mybutton, #click{})
           end,
           undefined %% We just want to verify that the postback worked.
         }.

```
    
##### Adding Tests to an existing page

     If you have an existing page on which you would like to conduct tests
     without cluttering up the module's page, instead building all the tests on
     a separate module. You can do this with `wf_test:start_other(ModuleName, TestFun)`.

     For example, say we want to test a module called `my_module` by creating a
     module called `test_module`:

```erlang
      -module(test_module).
      -compile(export_all).
      -include_lib("nitrogen_core/include/wf.hrl").

      main() -> wf_test:start_other(my_module, fun tests/0).

      tests() ->
        ?wf_test_js(fill_form, fill_form()).

      fill_form() ->
        {
          fun() ->
            wf:set(first_name, "Jesse"),
            wf:set(last_name, "Gumm"),
            wf:wire(button, #click{})
          end,
          "return objs('confirmation').text()",
          fun([Text]) ->
            Text == "Thank you for your submission"
          end,
          [{delay, 500}]
        }.

```

     This will load the `my_module` page, starting the tests in the process.

#### 3. Chain of pages

    The overarching implementation of the tests for an application is almost
    completely done in configurations.

    There are two configuration options you want to add to your `app.config`
    file: `test_browsers` and `tests`.

 *  `test_browsers` is a list of browsers in which to test your app.
 *  `tests` which is a list of paths to test.

```erlang
      {test_browsers, [
        "google-chrome",
        "firefox"
      ]},
      {tests, [
        "/path/to/test",
        "/path/to/other_test"
      ]},

```

    Once this is set up, you'll want to run `wf_test:start_all(AppName)` (where
    AppName, with the default setup is `nitrogen`).  This will launch all tests
    listed in the `tests` app var in order, and in each browser listed in
    `test_browsers`.

    If there is an `undefined` specified in `test_browsers`, then it will not
    open a browser, but instead expect you to load a specific URL in the
    browser of your choice to launch the tests.
