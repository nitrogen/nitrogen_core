

## Password Element - #password {}

  The password element produces an HTML masked password entry box.

### Usage

```erlang
   #password { id=password1 }

```

### Attributes

   * `disabled` (boolean) - Set to true to disable this element in the
      browser (sets the HTML `disabled` property, effectly greying out the
      element). Can be dynamically re-enabled with `wf:enable(ElementID)` or
      `wf:wire(ElementID, #enable{})` (Default: `false`)

   * `readonly` (boolean) - Set to true to make this element readonly in the
      browser. Can be toggled with `wf:wire(ElementID, #make_writable{})` and
      `wf:wire(ElementID, #make_readonly{})`.

   * `next` (atom) - 
      If set with a valid control ID, pressing the enter key in the 
      password box will automatically move focus to the specified control.

   * `postback` (Erlang term) - 
      If set, pressing the enter key in the password box will automatically
      initiate a Nitrogen postback with the supplied term.

   * `html_name` (string) - The name attribute of the password element.
### See Also

 *  [base element](./element_base.md)
 *  [button element](./button.md)
 *  [textbox element](./textbox.md)
 *  [textarea element](./textarea.md)
 *  [checkbox element](./checkbox.md)
 *  [dropdown element](./dropdown.md)
 *  [option element](./option.md)
 *  [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)
 
