

## Confirm Password Validator - #confirm_password {}

  When asking a user to enter a password (or other value) twice, confirm that the values match.

### Usage

```erlang
   wf:wire(ButtonID, ConfirmPasswordID, #validate { validators=[
     #confirm_password { text="Passwords must match.", password=PasswordID }
   ]})

```

### Attributes

   * `text` (string) - The text to display if validation fails.

   * `password` (atom) - Specify the Nitrogen Element ID of the Password textbox.

### See Also

 *  [Confirm Same Validator](confirm_same.md)

 *  [Validate Action](validate.md)

 *  [Validation Demos](http://nitrogenproject.com/demos/validation)
