

## General In-Place Element - #inplace {}

  The inplace element creates a view element that the user can edit in place.

  The user clicks on the view element, and it is replaced with an edit
  element, an OK button, and a Cancel button. 

  If the user clicks OK, a callback is issued to the delegate module
  so that it can save the new value.

### Usage

```erlang
   #inplace { id=textbox1, text="Some text.", view=#span{}, edit=#textbox{} }.

   #inplace { id=textbox2, text="", view=#image{}, edit=#textbox{} }.

   Dropdown = #dropdown{options=[#option{value="option1", text="Option 1"},
                                 #option{value="option2", text="Option 2"},
                                 #option{value="option3", text="Option 3"} ]},
   #inplace { id=textbox3, text="option2", view=Dropdown#dropdown{actions=#disable{}}, edit=Dropdown }.

```

### Attributes

   * `view` (Erlang term) - View element.

   * `edit` (Erlang term) - Edit element.

   * `text` (string) - Set the view element's contents.

 *  start_mode - (/Atom/, 'edit' or 'view') :: Set whether the textbox should start in edit mode or view mode.

   * `tag` (Erlang term) - See the delegate attribute for details.

   * `delegate` (Atom) - Name of the module to call inplace_event(Tag, Value).

### Callbacks

 *  inplace_event(Tag, Value) :: Called when the user presses the OK button.
	Tag is specified in the 'tag' attribute, above.  Value is the current value
	of the textbox. This callback function should process the value as
	necessary, and must return either the value itself, or a new value that will
	be sent back to the page and set as the value of the textbox.

### See Also

 *  [base element](./base.html)

 *  [inplace_textarea element](./inplace_textarea.html)

 *  [inplace_textbox element](./inplace_textbox.html)
