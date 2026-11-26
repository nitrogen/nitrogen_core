<!-- dash: #remove_option | Action | ##:Section -->

## Remove Option Action - `#remove_option{}`

This action tells the browser to remove elements from a `#dropdown{}` element.

### Usage

```erlang
wf:wire(#remove_option{target=my_dropdown, value="remove_value"}).

%% or

wf:wire(my_dropdown, #remove_option{value="remove_value"}).
```

### Attributes

* `target` (Element ID) - The target `#dropdown` field to remove the element from.
* `value` (String) - The value of the `#option` to remove from the dropdown list.

### See Also

* [base action](./action_base.md)
* [add option action](./add_option.md)
