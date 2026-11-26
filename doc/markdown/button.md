<!-- dash: #button | Element | ###:Section -->



## Button Element - #button {}

  The button element produces an HTML button. When clicked,
  the button will cause a Nitrogen postback.

### Usage

```erlang
   #button { text="Go!", postback={click, goButton} }

```

### Attributes

   * `body` (List of Nitrogen Elements) - The button's body (if you wanted
		to use something richer than a simple text)

   * `text` (string) - The text of the button.

   * `html_encode` (boolean) - Set to true to safely html-encode the text.

   * `disabled` (boolean) - Set to true to disable this button in the
		browser (sets the HTML `disabled` property, effectly greying out the
		element). Can be dynamically re-enabled with `wf:enable(ElementID)` or
		`wf:wire(ElementID, #enable{})` (Default: `false`)

   * `image` (Path to Image) - This is a shortcut attribute to prepending
		the body of the button with an image (for example, specifying a "Floppy
		Disk" icon on a "Save" button). The image will display before any body or
		text.

   * `postback` (Erlang term) - Clicking on the button will initiate a
		Nitrogen postback with the supplied term.

   * `click` (Action/ or /List of Actions) - Wires the selected actions to
		the \"click\" events.  Due to the commonality of binding actions to the
		click event, this is merely a shortcut for

```ERLANG
   #button { text="Do Something", actions=[
      #event{type=click,actions=ListOfActions}
   ]}

```

   * `enter_clicks` (List of Element IDs) - Wires to the provided ElementIDs
    a `#click{}` event on the provided button if the user presses the "Enter"
    key when any of the provided `ElementIDs` have the focus. To see a working
    example, see the [Security Demo](http://nitrogenproject.com/demos/security)

### See Also

 *  [Base](./element_base.md.md)
 *  [Link](./link.md)
 *  [Textbox](./textbox.md)
 *  [Password](./password.md)
 *  [Textarea](./textarea.md)
 *  [Checkbox](./checkbox.md)
 *  [Dropdown](./dropdown.md)
 *  [Dropdown Option](./option.md)
 *  [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)
 *  [Security Demo](http://nitrogenproject.com/demos/security)
     (showing the use of the `enter_clicks` attribute)
