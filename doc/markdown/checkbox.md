

## Checkbox Element - #checkbox {}

  The checkbox element produces an HTML checkbox.

### Usage

```erlang
   #checkbox { id=checkbox1, text="Check Me", checked=true }

```

### Attributes

   * `text` (string) - The checkbox's label.

   * `html_encode` (boolean) - Set to true to safely html-encode the text.

   *  `label_position` - (/'after', 'before', or 'none') :: Set the positioning
	  of the provided text label relative to the checkbox itself. If set to
	  'none', no text label will be rendered at all. Default: `'before'`. (**Note:**
	  `after` is a keyword in Erlang, so you /must/ ensure you wrap the after with
	  single quotes like 'after').

   * `postback` (Erlang term) - If set, clicking on the checkbox will
	  initiate a Nitrogen postback with the supplied term.

   * `checked` (boolean) - True if the checkbox should be checked.

   * `value` (string) - The value that will be posted if checked. The
	  default HTML behavior is the string "on"

   * `html_name` (string) - The name attribute of the checkbox.

### Special Note about HTML, Checkboxes, and Nitrogen posbacks

In HTML Forms, a checkbox element is only submitted if the checkbox is
/actually checked/. Nitrogen follows this pattern in its postbacks, which
might be undesirable in certain situations. To alleviate this, there is a
plugin called `always_checkbox`, which always submits.  It has not been
rolled into mainline Nitrogen yet, as its still in an experimental stage
(basically trying to get the API right).

[`#always_checkbox` on Github](https://github.com/choptastic/always_checkbox)

### See Also

 * [base](./element_base.md.md)
 * [Button](./button.md)
 * [Link](./link.md)
 * [Textbox](./textbox.md)
 * [Password](./password.md)
 * [Textarea](./textarea.md)
 * [Dropdown](./dropdown.md)
 * [Dropdown Option](./option.md)
 * [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)
