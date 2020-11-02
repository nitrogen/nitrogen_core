

## Required Field Validator - #is_required {}

  Validate that the field contains a value.

### Usage

```erlang
   wf:wire(ButtonID, TextBoxID, #validate { validators=[
     #is_required { text="Required" }
   ]})

```

### Attributes

   * `text` (string) - The text to display if validation fails.

   * `unless_has_value` (element id or list of element ids) - Don't fail
     this validation as long as the specified elements have values. This can be
     used for creating validators like "Please enter either a firstname or a
     company name."

### See Also

	*  [Validate Action](validate.md)

	*  [Validation Demos](http://nitrogenproject.com/demos/validation)
