<!-- dash: #custom | Test | ###:Section -->



## Custom Server-Side Validator - #custom {}

  Attach a custom validator, written in Erlang, to an element. The validator
  will be executed on the server during a postback.

### Usage

```erlang
   % Function to validate that a value is either upper or lower case,
   % depending on the value of Tag...
   F = fun(Tag, Value) ->
     case Tag of
       upper -> string:to_upper(Value) == Value;
       lower -> string:to_lower(Value) == Value
     end
   end,
	
   % Wire the validator in upper case mode...
   wf:wire(ButtonID, TextBoxID, #validate { validators=[
     %% If we wanted to validate lowercase, we could change
     %% tag to 'lower' and it would execute the 'lower' clause
     %% in the case statement above.
     #custom { text="Must be uppercase", function=F, tag=upper }
   ]})

```

### Attributes

   * `text` (string) - The text to display if validation fails.

   * `function` (fun) - An Erlang function that returns true if the value is
		valid, false if invalid. (`fun(Tag, Value) -> true | false`)

   * `tag` (Erlang term) - Tag value is passed into the custom validation
		function.

### See Also

	*  [JS Custom Action (client side)](js_custom.md)

	*  [Validate Action](validate.md)

	*  [Validation Demos](http://nitrogenproject.com/demos/validation)
