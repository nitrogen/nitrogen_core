

## DropDown Element - #dropdown {}

The dropdown element produces an HTML dropdown.

### Usage

```erlang
   #dropdown { id=dropdown1, value="2", options=[
     #option { text="Option 1", value="1" },
     #option { text="Option 2", value="2" },
     #option { text="Option 3", value="3" }
   ]}

```

### Attributes

   * `options` (list of option elements or {Value, Text} tuple) - Set the
		available options for this dropdown.

   * `html_encode` (boolean) - Set to true to safely html-encode the
		options' text.

   * `postback` (Erlang term) - If set, selecting an option will
		automatically initiate a Nitrogen postback with the supplied term.

   * `value` (string) - If set, the option matching this value will be
		selected by default.

   * `html_name` (string) - The name attribute of the dropdownbox

### See Also

 *  [base element](base_element)

 *  [option element](./option.html)

 *  [button element](./button.html)

 *  [textbox element](./textbox.html)

 *  [password element](./password.html)

 *  [textarea element](./textarea.html)

 *  [checkbox element](./checkbox.html)

 
