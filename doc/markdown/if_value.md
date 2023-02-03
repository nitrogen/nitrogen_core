## If Value Action - `#if_value{}`

This action allows Nitrogen to conditionally wire actions to the browser based
on the value of a form element. 

### Usage

#### To compare against a single value

```erlang
wf:wire(#if_value{
	target=food,
	value="banana",
	actions = #alert{text="Food is set to banana"},
	else = #alert{text="Food is not banana"}).
```

#### To compare against multiple values

```erlang
Map = [
	{"banana", #alert{text="It's a banana"},
	{"apple", #alert{text="It's an apple"}
],
Else = #alert{text="It's neither a banana, nor an apple"},

wf:wire(element_id, #if_value{map=Map, else=Else}).
```

### Attributes

* `target` (Element ID) - The ID of the form element to check the value of.
* `value` (Any) - The value to compare the target element against. Only used
  when comparing a single value.
* `actions` (List of actions) - The list of actions to wire if the target value
  is equal to the `value` attribute. Only used when comparing against a single
  value
* `else` (action or List of actions) - The list of actions to wire if the
  target value is not equal to the `value` attribute. Used in both usages.
  Optional in both usages.
* `map` (Proplist) - A proplists mapping a value to a list of actions. Only
  used when comparing against multiple values.

### See Also

* [base action](./action_base.md)
* [js\_custom action](./js_custom.md)
