

## Max Length Validator - #max_length {}

  Validate the maximum length of text in a textbox.

### Usage

```erlang
   wf:wire(ButtonID, TextBoxID, #validate { validators=[<br>
     #max_length { text="Maximum of 7 characters.", length=7 }<br>
   ]})

```

### Attributes

   * `text` (string) - The text to display if validation fails.

   * `length` (integer) - The maximum length of text.

### See Also

	*  [Validate Action](validate.md)

	*  [Validation Demos](http://nitrogenproject.com/demos/validation)
