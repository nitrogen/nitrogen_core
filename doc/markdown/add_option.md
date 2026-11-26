## Add Option Action - `#add_option{}`

This action tells Nitrogen to add elements to a #dropdown field. 

### Usage

```erlang
NewOption = #option{
	value = "new_value",
	text = "The new item"
},
wf:wire(my_dropdown, #add_option{option=Option, location=top}).
```

### Attributes

* `target` (Element ID) - Which `#dropdown{}` to add the item to.
* `option` (Option) - The `#option` element to add to the `#dropdown{}` list.
* `location` (Atoms: `top` or `bottom`) - The location to insert the new
  option, `top` of the list or `bottom` of the list.

### See Also

* [base action](./action_base.md)
* [remove option action](./remove_option.md)
