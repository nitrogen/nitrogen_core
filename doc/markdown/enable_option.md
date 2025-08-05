## Enable Option Action - `#enable_option{}`

This action tells Nitrogen to enable an item in a `#dropdown` field.

### Usage

```erlang
wf:wire(#enable_option{target=my_dropdown, value="item_to_enable"}).

%% or

wf:wire(my_dropdown, #enable_option{value="item_to_enable"}).
```

### Attributes

- `target` (Element ID) - Which `#dropdown{}` contains the item you want to
  enable.
- `value` (String) - The `value` of the `#option` you wish to enable in the
  `#dropdown{}` list.

### See Also

- [base action](./action_base.md)
- [`#disable_option` action](./disable_option.md)
