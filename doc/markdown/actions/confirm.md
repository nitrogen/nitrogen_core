

## Confirm Action - #confirm {}

  This action tells Nitrogen to trigger a Javascript confirmation box. (ok/cancel).

### Usage

```erlang
   wf:wire(#confirm { text="Do you want to continue?", postback=continue })

```

### Attributes

   * `text` (string) - The text of the confirm box.

   * `postback` (Erlang term) - If 'ok' is clicked, Nitrogen will issue a postback with this term.

### Callbacks

#### event(Tag)

    Called when the ok button is clicked. Tag is specified in the 'postback' attribute.

### See Also

 *  [base element](./base.html)

 *  [alert element](./alert.html)

 
