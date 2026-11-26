## Click Action - `#click{}`

This action tells Nitrogen to simulate a user click on an element.

### Usage

```erlang
wf:wire(#click{target = my_button}).

%% or

wf:wire(my_button, #click{}).
```

### Attributes

* `target` (Element ID) - The target element to simulate a user click on.

### See Also

* [base action](./action_base.md)
