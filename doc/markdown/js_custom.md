<!-- dash: #js_custom | Test | ###:Section -->



## Custom Client-Side Validator - #js_custom {}

Attach a custom validator, written in Javascript, to an element. The
validator will be executed on the client when a postback is
triggered. Validation failure will prevent the postback.

See [Live Validation's Validate
Custom](http://livevalidation.com/documentation#ValidateCustom) for more
information about the validation function and args.

### Security Note

Note that because this is a purely Javascript-based validation, that you can't
trust this validator in the same way you can trust that the server-side
validators will ensure that a value is valid prior to postback.  You will still
likely have to verify your values server-side to protect against tampering.

This validator is provided more to give faster response to your users, since
Javascript doesn't have to communicate with the server to validate. It usually
goes together with the `#custom` server-side validator.

### Usage

```erlang
   % Wire the validator against the 'my_validator' Javascript
   % function, passing in some args...
   wf:wire(ButtonID, TextBoxID, #validate { validators=[
     #js_custom {
       text="Validation failed.",
       function=my_validator,
       args="{ amount: 5 }"
     }
   ]})

```

```erlang
   wf:wire(ButtonID, TextBoxID, #validate { validators=[
     #js_custom {
       text="Validation failed.",
       function="function(value, args) { return value == 'the only correct answer'} ",
       when_empty=true }
   ]})

```

### Attributes

* `text` (string) - The text to display if validation fails.

* `function` (atom or string) - The name of a **Javascript** function of an
  anonymous javascript function that returns `true` if the value is valid, `false`
  if invalid.

  `function(Value, Args) -> 'true' or 'false'`

* `args` (JSON String) - Extra arguments passed to the custom validation
  function.

* `when_empty` (boolean) - Run the validator even if the specified
  field's value is empty. By default, the custom validator doesn't apply if
  the field is empty, since you would normally use a separate `#is_required`
  for that. (default: `false`)

### See Also

	*  [Custom Action (server side)](custom.md)
	*  [Validate Action](validate.md)
	*  [Validation Demos](http://nitrogenproject.com/demos/validation)
