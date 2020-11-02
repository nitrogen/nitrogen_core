

## Email Validator - #is_email {}

  Validate that the field contains a valid email address.

### Usage

```erlang
   wf:wire(ButtonID, TextBoxID, #validate { validators=[
     #is_email { text="Not a valid email address." }
   ]})

```

### Attributes

   * `text` (string) - The text to display if validation fails.

### See Also

	*  [Validate Action](validate.md)

	*  [Validation Demos](http://nitrogenproject.com/demos/validation)
