## Set Multiple Action - `#set_multiple{}`

Sets the values for a multi-select `#dropdown` element or an HTML `<select
multiple>` tag.

### Usage

```erlang
	wf:wire(my_dropdown, #set_multiple{values=["value1", "value2", "value3"]}).
```

### Attributes

* `values` (List) - List of values to be selected in the multi-select
  `#dropdown{multiple=true}` element.

### See Also

* [base action](./action_base.md)
* [set action](./set.md)
* [dropdown element](./dropdown.md)
