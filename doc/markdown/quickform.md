<!-- dash: #BASIC | Element | ###:Section -->

## `#quickform{}` Element

The `#quickform{}` element can assist you in rapidly generating a series of
form fields, and contains a pile of shortcuts for a variety of form fields, as
well as pre-filling default values (for example, pre-filling a customer's name
and shipping information to a checkout page).

### Usage

```
 Data = #{
    first_name => "Michael",
    last_name => "Bluth",
    company => "Bluth Company",
    start_date => "2003-11-02",
    employment_type => full,
    currently_employed => false
},

EmploymentTypes = [
    {full, "Full Time"},
    {part, "Part Time"},
    {self, "Self Employed"},
    {student, "Student"},
    {never, "Never Employed"}
],

Fields = [
    {first_name, "First Name"},
    {last_name, "Last Name"},
    #quickform_group{
        header="Employment Details"},
        fields=[
            {company, "Current (or most recent) employer"},
            {start_date, date},
            {currently_employed, yesno},
            {dropdown, employment_type, {dropdown, EmploymentTypes}}
        ]
    }
],

[
    #quickform{
        data=Data,
        fields=Fields
    },
    #button{postback=save, text="Save"}
].
```

### Attributes

- `data` (Key-value list, Map, or other Data structure) - The value of this
  attribute is expected to be either a key-value tuple list, an Erlang map, or
  any data structure that can be understood by
  [`erlang_ds`](https://github.com/choptastic/erlang_ds). This will be used to
  pre-populate values into the `fields` attribute listed below.
- `fields` (List of [short fields](#short_fields), Nitrogen elements, or the
  atom `'-'`) - The `fields` attribute is what does the bulk of the work
  generating form fields. This value must be a list of either [short
  fields](#short_fields), Nitrogen elements, or the atom `'-'`. Nitrogen
  elements will be presented as-written (with no additional processing from the
  `#quickform{}` element's renderer). The atom `'-'` will be converted to an
  `#hr{}` element. The full list of short fields is below:

#### <a name=short_fields></a>Short Field Definitions

All short fields for quicktable are defined as either 2, 3, or 4 element tuples.

The brief description of the format can be found below:

`{FieldID, Label, Type, Options}`

- `FieldID` (atom) - The ID of the form field
- `Label` (text) - The text of the label for the field.
- `Type` (a variety of atoms and tuples) - This field determines what _kind_ of
  field this will be (textbox, dropdown box, etc). The available options are:
  - `textbox` - A `#textbox{}`. Available options: `placeholder`
  - `textarea` - A `#textarea`. Available options: `placeholder`, `rows`, `columns`
  - `date` - A `#textbox` with type `date`, which will use the browser's
    built-in date seleector.
  - `datepicker` - A `#datepicker_textbox`. Avaiable options: `placeholder`
  - `date_dropdown` - A `#date_dropdown{}` element. Available options: `format`
    (see [`#datepicker_textbox{}`](datepicker_textbox.md) for the valid
    options. Specifically, look at the `format` attribute on that element's
    page - note, `#datepicker_textbox` element is deprecated.)
  - `{dropdown, DropdownOptions}` - A [`#dropdown{}`](dropdown.md) box. The
    value of `DropdownOptions` can be anything acceptable to to the
    `#dropdown.options` attribute (a list containing either `{Value, Text}`,
    `#option{}` or`#option_group{}`- see [`#dropdown{}`
    documentation](dropdown.md) for more details.
  - `{year, Min, Max}` - A dropdown box of possible years from `Min` to `Max`.
    `Min` or `Max` can be the atom `now` for convenience.
  - _`any atom or string`_ - Finally, if the `Type` is any other term (an atom
    or string from above), then it will render as `#textbox{type=Type}`. This,
    as with other text-like fields above, supports the `placeholder` option.
- `Options` (it depends on the `Type`) - This field should typically be a
  key-value tuple list providing a variety of options. The actual available
  options are listed above with each `Type`.

Finally, the 2 and 3 element versions work as follows:

- `{FieldID, Label, Type}` is the same as `{FieldID, Label, Type, []}`, or in
  prose: no options.
- `{FieldID, Label}` is the same as `{FieldID, Label, textbox, []}`, or in
  prose: just a textbox, no options.

### See Also

- [Demo: quickform](https://nitrogenproject.com/demos/quickform)
- [textbox element](textbox.md)
- [textarea element](textarea.md)
- [datepicker_textbox element](datepicker_textbox.md)
- [date_dropdown element](date_dropdown.md)
- [button element](button.md)
- [prompt action](prompt.md)
- [base element](element_base.md)
