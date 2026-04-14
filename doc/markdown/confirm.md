<!-- dash: #confirm | Event | ###:Section -->

## Confirm Action - `#confirm{}`

This action tells Nitrogen to trigger a confirmation box, and can be executed
either as a standard Javascript confirm box or as an "enhanced" box (using
Nitrogen's `#modal{}` action.

### Standard Usage

This standard usage is a shortcut for the [`#modal{}`](modal.md) action with an
"OK" button (and a insteaad of "Close", it says "Cancel" to match the behavior
of `window.confirm()`). Because it is an expanded `#modal{}`, it will use
whatever you've configured with the [modal_handler](modal_handler.md) if you've
customized that).

```erlang
wf:wire(#confirm{
    title_text="Next Step",
    text="Do you want to continue?",
    ok_text="Yes, Continue",
    close_text="No, do nothing and close this window",
    postback=continue,
})
```

### Basic Usage

The basic usage will trigger a JavaScript
[`confirm()`](https://developer.mozilla.org/en-US/docs/Web/API/Window/confirm)
box (with OK and Cancel buttons)

```erlang
wf:wire(#confirm{
    text="Do you want to continue?",
    postback=continue,
    basic=true
})
```

It's important to note that because this "basic" approach relies on the
JavaScript `window.confirm()`, that JavaScript Execution halts while the popup
is visible.

### Attributes

- `basic` (boolean) - If `true`, this will issue a javascript
  `window.confirm()` call.

- `text` (string) - The text of the confirm box.

- `body` (elements) - The body of the confirm box.

- `title_text` (string) - The text of title box of the popped up modal.
  Typically, if this is specified, `title_body` would be left blank.

- `title_body` (elements) - The body of the title box of the popped up modal.
  Typically, if this is specified `title_text` would be left blank.

- `ok_text` (string) - The text of the "OK" button.

- `ok_body` (elements) - The body of the "OK" button.

- `postback` (Erlang term) - If 'ok' is clicked, Nitrogen will issue a postback
  with this term assigned to this attribute. If `postback` is `undefined`,
  there will be no "OK" button created by default. Instead, it expects that
  you've defined the `buttons` attribute.

- `buttons` (list of [`#button{}`](button.md) elements or button shortcut
  tuples) - While the attributes above are generally sufficient for most uses,
  sometimes you want more granular control over the buttons. In that case, the
  `buttons` attribute allows you to modify the buttons, or add as many buttons as
  you like. The value assigned to the `buttons` attribute should be a list, with
  each item in the list being either a `#button{}` element, or a shortcut tuple
  of `{Text, Postback}` or `{Text, Postback, Delegate}`. This list can also be a
  _mix_ of Nitrogen elements and shortcut tuples.

### Callbacks

#### event(Tag)

Called when the ok button is clicked. `Tag` is specified in the 'postback'
attribute.

### See Also

- [base element](./action_base.md)
- [modal action](modal.md)
- [prompt action](prompt.md)
- [alert action](./alert.md)
- [modal handler](modal_handler.md)
- [External: `window.confrm()`](https://developer.mozilla.org/en-US/docs/Web/API/Window/confirm)
