<!-- dash: #is_integer | Test | ###:Section -->



## Integer Validator - #is_integer {}

  Validate that the field contains an integer.

### Usage

```erlang
   wf:wire(ButtonID, TextBoxID, #validate { validators=[
     #is_integer {
       min=1,
       max=100,
       text="Must be an integer between 1 and 100."
     }
   ]})

```

### Attributes

   * `text` (string) - The text to display if validation fails.

   * `min` (integer) - If set, will be the minimum acceptable value for the
   field. If left blank, then no maximum will be enforced.

   * `max` (integer) - If set, will be the maximum acceptable value for the
   field. If left blank, then nno maximum will be enforced.

### See Also

 *  [Validate Action](validate.md)

 *  [Validation Demos](http://nitrogenproject.com/demos/validation)
