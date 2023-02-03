## Range Element - `#range{}`

The `#range{}` element creates an HTML `<input type="range">` tag. 

### Usage

```erlang
#range{
	min = 0,
	max = 100,
	step = 1,
	value = 50,
	postback = #postback{
		function = set_value,
		vessel = my_vessel
	}
}.
```

### Attributes

* `min` (Integer) - The minimum value for the range slider.
* `max` (Integer) - The maximum value for the range slider.
* `step` (Integer) - The step size between values on the range slider. (default: 1)
* `value` (Integer) - The initial value for the range slider.
* `postback` (Postback) - The postback to be triggered when the range slider is moved. (optional)
* `vessel` (Nitrogen ID or CSS selector) - The vessel to limit the postback's scope. (optional)

### Example

In the following example, a range slider is created with a minimum value of 0,
a maximum value of 100, a step size of 1, and an initial value of 50. When the
slider is moved, it will initiate a postback with the atom `my_postback`.

```markdown
>>>erlang
#range{
	min = 0,
	max = 100,
	step = 1,
	value = 50,
	postback = my_postback
}.
>>>

### See Also

* [base element](./element_base.md)
