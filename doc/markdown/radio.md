
## Radio Element - #radio {}

  The radio element produces an HTML radio button.

### Usage

```erlang
   [
      #radio { name=fruit, text="Apple", checked=true },
      #radio { name=fruit, text="Orange" },
      #radio { name=fruit, text="Banana", value="banana" }
   ].

```

### Attributes

   * `body` (Nitrogen element or list of Nitrogen Elements) - Radio button label either in HTML or Nitrogen elements.

   * `text` (string) - The radio button's label.

   * `html_encode` (boolean) - Set to true to safely html-encode the text label.

   * `value` (string) - The value to be submitted for this radio button in postbacks if it is selected.

 *  label_class - (\atom, string or list of atoms and strings\) :: A class applied to the connected `<label>` element if `text` or `body` are provided.

   * `postback` (Erlang term) - If set, clicking on the checkbox will initiate a Nitrogen postback with the supplied term.

   * `checked` (boolean) - True if the radio button should be checked.

   * `name` (string) - The name attribute of the radio button. If this is not contained within a `#radiogroup{}` element, then `name` must be specified, as `name` determines the "group" of the radio buttons (that is, when one member of a radio group is selected, the others are unselected).

   * `html_name` (string) - The name attribute of the checkbox. ( This does essentially the same thing as the `name` attribute, but is semantically different for use with the RESTful elements, as all RESTful elements shre the `html_name` attributes. If you're not sure, just use `name`)

### See Also 

 *  [base](./element_base.md)
 *  [radiogroup](./radiogroup.md)
 *  [Button](./button.md)
 *  [Link](./link.md)
 *  [Textbox](./textbox.md)
 *  [Password](./password.md)
 *  [Textarea](./textarea.md)
 *  [Dropdown](./dropdown.md)
 *  [Dropdown Option](./option.md)
 *  [RESTful element overview](/restful_overfiew.md)
 *  [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)
 *  [Radio Buttons Demo](http://nitrogenproject.com/demos/radio)
