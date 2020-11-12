
## Clear Validation Action - #clear_validation {}

This action tells Nitrogen to remove the validation requirements from the page.
It can remove validators en masse.

### Usage

```erlang
   wf:wire(#clear_validation{ validation_trigger = my_button }).

```

### Attributes

   * `validation_target` (Nitrogen ID) - Validation target is the ID of the
      field (textbox, dropdown, etc) that's being validated. Set this to remove
      validations from specific form fields (ie, fields that are being removed during
      postbacks).

   * `validation_trigger` (Nitrogen ID) - Validation target is the ID of the
      element initiating the validation (ie `#button{}` or `#link{}` elements). Set this
      to have all validations triggered by that element removed.

   * `validation_all` (Nitrogen ID) - This is the nuclear option for
      removing validation. Set this to any Nitrogen element to have any validators
      connected to this element removed, either as a trigger or a target.
### Wiping all validations from the page

This element can also be used to wipe all validations from all elements on the
page.  This is done by simply leaving the above three fields blank (or setting
them to `undefined`).

For example:

```erlang
   wf:wire(#clear_validation{}). %% Wipe out all validation on the page

```


### See Also

 *  [base action](./action_base.md)

 *  [validate action](./validate.md)

 *  [Clear Validation Demo](http://nitrogenproject.com/demos/clear_validation)
