## Comet Action - `#comet{}`

Enables asynchronous processing with Nitrogen's Comet feature.

### Usage

```erlang
	wf:wire(#comet{
		function = fun comet_loop/0}
		pool = my_pool,
		scope = local,
	}).

	comet_loop() ->
		wf:update(my_time_element, qdate:to_string("Y-m-d h:i:s")),
		wf:flush(),
		timer:sleep(1000),
		comet_loop().
```

This action will start the `comet_loop/0` function in the background as part of
the pool name `my_pool` with local scope. If the comet connection dies, the message
`"Comet connection has died."` will be displayed.

The `comet_loop/0` function just updates the time element with the current
date and time, flushes the actions to the browser, waits for a second, and then
loops back to itself.

### Attributes

* `function` (function with arity 0) - An Erlang function that executes in the
  background. The function generates actions that are then sent to the browser
  via the accumulator.

* `pool` (Atom) - A pool name for the AsyncFunction process.

* `scope` (Atom) - Specifies the scope of the pool. Can be either `local` or
  `global`. A `local` pool applies only to the current series of page requests
  by a user, while a `global` pool applies to the entire system.

* `dying_message` (Any Erlang Term) - A message to be sent to the member functions if
  the function dies.

* `reconnect_actions` (String, Action, or List or Actions) - Nitrogen actions
  or JavaScript to be executed when reconnecting to the page after a
  disconnection.  Typically, it might be something like
  `#event{postback=attempt_reconnect}` to trigger a postback to try to
  reconnect to the comet.

### Notes

Generally speaking, you usually want to start a comet process with `wf:comet`
or `wf:comet_global`. See Nitrogen's [Function Reference](./api.md)

### See Also

* [base action](./action_base.md)
* [delay_body element](./delay_body)
* [Comet Demo1](https://nitrogenproject.com/demos/comet1)
* [Comet Demo1](https://nitrogenproject.com/demos/comet2)
* [Comet Demo1](https://nitrogenproject.com/demos/comet3)
* [sync_panel demo](https://nitrogenproject.com/demos/sync_panel)
* [delay_body demo](https://nitrogenproject.com/demos/delay_body)

