<!-- dash: #validate | Event | ###:Section -->


## Validate Action - `#validate{}`

  This action tells Nitrogen to validate input fields during a postback or blur event.

### Usage

```erlang
   wf:wire(myButton, myTextbox, #validate { validators=[
     #is_required { text="Required." }
   ]})

```

### Attributes

   * `on` (atom, submit by default) - Set to 'submit' to validate fields during a postback. Set to 'blur' to validate when the field loses focus.

   * `success_text` (String) - Text to display next to a field when the field passes validation.

   * `validators` (Validator, or List of Validators) - The validators to run on this field.

   * `attach_to` (atom) - Specify the id of a Nitrogen element to attach to. By default, the validator will attach to the target of the action.

### See Also

 *  [base action](./action_base.md)

 *  [clear validation action](./clear_validation.md)
