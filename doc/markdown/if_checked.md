<!-- dash: #if_checked | Event | ###:Section -->

## If-Checked Action - `#if_checked{}`

This action allows Nitrogen to conditionally wire actions to the browser based
on the whether the identified checkbox is checked or not.

### Usage

#### To compare against a single value

```erlang
wf:wire(#if_checked{
    target=agree,
    actions = #alert{text="You have agreed"},
    else = #alert{text=You have not agreed"}
}).
```

### Attributes

- `target` (Element ID) - The ID of the form element to check its `checked`
  status.
- `actions` (List of actions) - The list of actions to wire if the target
  element is checked.
- `else` (action or List of actions) - The list of actions to wire if the
  target element is checked attribute.

### See Also

- [base action](./action_base.md)
- [if_value](./if_value.md)
- [js_custom action](./js_custom.md)
