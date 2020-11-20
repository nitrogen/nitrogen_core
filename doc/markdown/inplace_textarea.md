<!-- dash: #inplace_textarea | Element | ###:Section -->


## In-Place Textarea Element - #inplace_textarea {}

  The inplace_textarea element creates a textarea that the user can edit in
  place.

  The user clicks on the label, and it is replaced with a textarea, an OK
  button, and a Cancel button. 

  If the user clicks OK, a callback is issued to the delegate module so that it
  can save the new text.

### Usage

```erlang
   #inplace_textarea { id=textarea1, text="Some text." }

```

### Attributes

   * `text` (string) - Set the textarea's contents.

   * `html_encode` (boolean) - Set to true to safely html-encode the text.

   * `start_mode` (Atom, 'edit' or 'view') - Set whether the textarea
      should start in edit mode or view mode.

   * `validators` (Validator, or list of validators.) - Validators to apply
      to the textarea.

   * `tag` (Erlang term) - See the delegate attribute for details.

### Callbacks

 *  inplace_textarea_event(Tag, Value) :: Called when the user presses the
      OK button. Tag is specified in the 'tag' attribute, above.  Value is the
      current value of the textarea. This callback function should process the
      value as necessary, and must return either the value itself, or a new value
      that will be sent back to the page and set as the value of the textarea.

### See Also 

 *  [base element](./element_base.md)

 *  [inplace_textbox element](./inplace_textbox.md)

 *  [inplace element](./inplace.md)

 *  [textarea element](./textarea.md)

 
