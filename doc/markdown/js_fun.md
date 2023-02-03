## JS Function Action - `#js_fun{}`

This action calls a JavaScript function that has already been defined.

### Usage

```erlang
wf:wire(#js_fun{function = "existingFunction", args = [arg1, arg2]}).
```

In JavaScript, this would be equivalent to calling:

```javascript
existingFunction(arg1, arg2);
```

### Attributes

* `function` (Atom) - The name of the existing JavaScript function to be called.
* `args` (List) - A list of arguments to pass to the JavaScript function. Can contain strings, atoms, or integers.

### See Also

* [base action](./action_base.md)
* [`#js_custom` Action](./js_custom.md)
* [`#if value` Action](./if_value.md)
