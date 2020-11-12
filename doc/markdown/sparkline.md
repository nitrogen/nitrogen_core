

## Sparkline Element - #sparkline{}

  A sparkline is a small graph which can be used in dashboards or inline with
  text. They can be presented in several different ways, as described in the
  Sparkline documentation.

### Usage

```erlang
	#sparkline{
		type=bar,
		values=[1,5,4,8,3,2]
	}

```

### Attributes

   * `type` (atom) - The type of sparkline to draw.  Can be `line`, `bar`,
	`tristate`, `discrete`, `bullet`, `pie`, `box`. (Default: `line`)

   * `values` (list of integers) - The data for the sparkline. Just a list
	of integers.

   * `options` (proplist) - A list of options using a standard Erlang
	proplists (a list of 2-tuples, like `{Option, Value}`.

### See Also

 *  [base element](./element_base.md)

	*  [Sparkline Demonstration](http://nitrogenproject.com/demos/sparkline)

	*  [Official Sparkline Documentation](http://omnipotent.net/jquery.sparkline)
