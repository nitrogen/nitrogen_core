## Continue Action - `#continue{}`

A Nitrogen action that runs a long-running function, and then passes the return
value to a specified module's `continue/2` function.

### Usage

```erlang
wf:wire(#continue{function=fun my_long_running_function/0, tag=my_tag}).

my_long_running_function() ->
	%% Do something that takes a long time.
	%% Sleeping takes a long time, we'll try that
	timer:sleep(30000),

	%% Time to return the answer to the 
	42.

continue(my_tag, Result) ->
	Msg = wf:f("The result is ~p",[Result]),
	Alert = #alert{text=Msg},
	wf:wire(Alert).
```

### Attributes

* `function` (Function) - An arity-0 Erlang function that is a long-running
  function. When the function finally returns, the return value is passed to
  the current page module as `Module:continue(Tag, Result)`.

* `tag` (Erlang Term) - An Erlang term that will be passed to the
  `Module:continue/2` function.

* `delegate` (Module) - A delegate module that will handle the return value of
  the `function` with the `continue/2` function. If no delegate is specified,
  the default is the current page module.

### See Also

* [Base Action](./action_base.md)
* [Continuations Demo](https://nitrogenproject.com/demos/continuations)
* [wf:continue()](./api.md#wf:continue)
