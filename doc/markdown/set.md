## Set Action - `#set{}`

This action tells Nitrogen to set the value of a form element, change the `src` attribute of an image, update a `#mermaid` element's contents, or set the value of a `#progress_bar` element.

### Usage

```erlang
wf:wire(my_textbox, #set{value = "new_value"}).
```

### Attributes

* `target` (Element ID) - Identifies the element to be set.
* `value` (Any) - The value to be set for the element.

### See Also

* [base action](./action_base.md)
* [wf:set](./api.md)
