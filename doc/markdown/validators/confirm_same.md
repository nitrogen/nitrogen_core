

## Confirm Same Validator - #confirm_same {}

  A general validator to verify that two fields have the same value. This could
  be used for validating "confirm email" or "confirm username" type fields.

### Usage

```erlang
   wf:wire(button, email, #validate { validators=[
     #confirm_same { text="Emails do not match", confirm_id=confirm_email }
   ]}),
   
   #label{text="Email Address"},
   #textbox{id=email},

   #label{text="Confirm Email"},
   #textbox{id=confirm_email},

   #button{id=button, text="Confirms"}

```

### Attributes

   * `text` (string) - The text to display if validation fails.

   * `confirm_id` (atom) - Specify the Nitrogen Element ID of the field
      against which you wish to confirm.

### See Also

 *  [Confirm Password Validator](confirm_password.html)

 *  [Validate Action](validate.html)

 *  [Validation Demos](http://nitrogenproject.com/demos/validation)
