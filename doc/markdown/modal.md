<!-- dash: #confirm | Event | ###:Section -->

## Modal Action - `#modal{}`

This action tells Nitrogen to trigger a customizable modal popup box (not a
popup window).

### Usage

```erlang
wf:wire(#modal{
    title_text="What are your plans?",
    body="What do you plan on doing today?",
    buttons=[
        {"Working", working},
        {"Playing Video Games", video_games},
        {"Cleaning", cleaning}
    ],
    close_text="Cancel"
}).
```

### Attributes

- `text` (string) - The text of the confirm box.

- `body` (elements) - The body of the confirm box.

- `title_text` (string) - The text of title box of the popped up modal.
  Typically, if this is specified, `title_body` would be left blank.

- `title_body` (elements) - The body of the title box of the popped up modal.
  Typically, if this is specified `title_text` would be left blank.

- `buttons` (list of [`#button{}`](button.md) elements or button shortcut
  tuples) - While the attributes above are generally sufficient for most uses,
  sometimes you want more granular control over the buttons. In that case, the
  `buttons` attribute allows you to modify the buttons, or add as many buttons as
  you like. The value assigned to the `buttons` attribute should be a list, with
  each item in the list being either a `#button{}` element, or a shortcut tuple
  of `{Text, Postback}` or `{Text, Postback, Delegate}`. This list can also be a
  _mix_ of Nitrogen elements and shortcut tuples.

- `show_close_button` (boolean) - If a "Close" button should be added
  automatically.

- `close_text` (string) - The text of the "Close" button, if visible.
  Typically, if this is specified, `close_text` will be left blank.

- `close_body` (elements) - The body of the "Close" button, if visible.
  Typically, if this is specified, `close_text` will be left blank.

### Callbacks

#### `event(Tag)`

Called when any button other than the "Close" button is clicked. As with all
postbacks, `Tag` is the contents of the `Postback` as specified in the
`buttons` attribute (either the "shortcut tuples" or any elements with
`postback` attributes).

### Implementation Details

While there is a `modal_action.erl` file, it's just calling functions in
`modal_handler.erl`, which, like any Nitrogen handler, calls functions in
[`default_modal_handler.erl`](https://github.com/nitrogen/nitrogen_core/blob/master/src/handlers/modal/default_modal_handler.erl).

### See Also

- [base element](./action_base.md)
- [modal action](modal.md)
- [prompt action](prompt.md)
- [alert action](./alert.md)
- [modal handler](modal_handler.md)
- [External: `window.confrm()`](https://developer.mozilla.org/en-US/docs/Web/API/Window/confirm)
