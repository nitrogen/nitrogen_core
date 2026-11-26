<!-- dash: #min_length | Test | ###:Section -->



## Min Length Validator - #min_length {}

  Validate the minimum length of text in a textbox.

### Usage

```erlang
   wf:wire(ButtonID, TextBoxID, #validate { validators=[<br>
     #min_length { text="Minimum of 4 characters.", length=4 }<br>
   ]})

```

### Attributes

   * `text` (string) - The text to display if validation fails.

   * `length` (integer) - The minimum length of text.

### See Also

	*  [Validate Action](validate.md)

	*  [Validation Demos](http://nitrogenproject.com/demos/validation)
