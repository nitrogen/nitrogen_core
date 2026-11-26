## Disable Option Action - `#disable_option{}`

This action tells Nitrogen to disable an item in a #dropdown field. 

### Usage

```erlang
wf:wire(#disable_option{target=my_dropdown, value="item_to_disable"}).

%% or

wf:wire(my_dropdown, #disable_option{value="item_to_enable"}).
```

### Attributes

* `target` (Element ID) - Which `#dropdown{}` to disable the item from.
* `value` (String) - The value of the `#option` to disable in the
  `#dropdown{}` list.

### See Also

* [base action](./action_base.md)
* [`#enable_option` action](./enable_option.md)
