<!-- dash: #inplace_textbox | Element | ###:Section -->


## In-Place Textbox Element - #inplace_textbox {}

  The inplace_textbox element creates a textbox that the user can
  edit in place.

  The user clicks on the label, and it is replaced with a textbox, an OK button, and a
  Cancel button. 

  If the user clicks OK, a callback is issued to the delegate module
  so that it can save the new text.

### Usage

```erlang
   #inplace_textbox { id=textbox1, text="Some text." }

```

### Attributes

   * `text` (string) - Set the textbox's contents.

   * `html_encode` (boolean) - Set to true to safely html-encode the text.

   * `start_mode` (Atom, 'edit' or 'view') - Set whether the textbox should
      start in edit mode or view mode.

   * `validators` (Validator, or list of validators.) - Validators to apply
      to the textbox.

   * `tag` (Erlang term) - See the delegate attribute for details.

### Callbacks

 *  inplace_textbox_event(Tag, Value) :: Called when the user presses the OK
      button. Tag is specified in the 'tag' attribute, above.  Value is the
      current value of the textbox. This callback function should process the
      value as necessary, and must return either the value itself, or a new value
      that will be sent back to the page and set as the value of the textbox.

### See Also

 *  [base element](./element_base.md)

 *  [inplace_textarea element](./inplace_textarea.md)

 *  [inplace element](./inplace.md)

 *  [textbox element](./textbox.md)

 
