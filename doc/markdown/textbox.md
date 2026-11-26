<!-- dash: #textbox | Element | ###:Section -->


## Textbox Element - `#textbox{}`

  The textbox element produces an HTML textbox.

### Usage

```erlang
   #textbox{id=textbox1, text="Some text.", next=textbox2}
```

### Attributes

 * `text` (string) - Set the textbox's contents.
 
 * `html_encode` (boolean) - Set to true to safely html-encode the text.
 
 * `maxlength` (integer) - Specify the maximum number of characters that
    can be typed by the user.
 
 * `size` (integer) - Specify the width of the textbox in characters. The
    HTML Default is `20`. Keep in mind, CSS Rules will override this setting.
 
 * `disabled` (boolean) - Set to true to disable this textbox in the
    browser (sets the HTML `disabled` property, effectly greying out the
    element). Can be dynamically re-enabled with `wf:enable(ElementID)` or
    `wf:wire(ElementID, #enable{})` (Default: `false`)
 
 * `readonly` (boolean) - Set to true to make this element readonly in the
    browser. Can be toggled with `wf:wire(ElementID, #make_writable{})` and
    `wf:wire(ElementID, #make_readonly{})`.
 
 * `placeholder` (string) - Sets the placeholder text (text that will be
    in the textbox until the user focuses in the textbox). This uses the
    HTML5 placeholder attribute. This attribute is not supported on very old
    browsers (Internet Explorer 9, Firefox 3, etc).  If you must add support,
    you can look into adding some
    [javascript or jquery plugins](https://github.com/mathiasbynens/jquery-placeholder)
    to your template.

 * `type` (atom|string) - Sets the input box "type" attribute. This is
    especially useful for taking advantage of the newer HTML5 values for
    this. It can be any of the following: `search`, `email`, `range`, `url`,
    `tel`, `number`, `date`, `month`, `week`, `time`, `datetime`,
    `datetime-local`, `color`, as well as the original HTML attributes,
    `text`, `button`, `submit`, `image`, and `hidden`.  (Default: `text`)

    [About HTML5 Input Types](http://html5doctor.com/html5-forms-input-types/)

    If `type` is set to a date or time value, the `text` attribute will be
    parsed with `qdate` to automatically set the value to a correctly
    formatted value.

 * `pattern` (string) - Assigns the HTML `pattern` attribute.  This is a
   regular expression that controls the values of the form.  For example, you
   could enforce a US-Style phone number by specifying `\d{3}-\d{3}-d\{4}`
   (which would succeed on a value like `"111-222-4567"`).  Bear in mind, this
   is only client-side enforcement, and doesn't currently stop postbacks from
   happening. But it can provide an cheap and fast client-side verification
   method.

 * `next` (atom) - 
    If set with a valid control ID, pressing the enter or tab key in the 
    textbox will automatically move focus to the specified control.

 * `postback` (Erlang term) - 
    If set, pressing the enter key in the textbox will automatically
    initiate a Nitrogen postback with the supplied term.

 * `min` (Erlang Term) - Assign the `min` value to the textbox (as defined by
   some of the HTML5 Input Types). As with the `value` attribute, if the
   textbox's `type` is set to a date or time format, the `max` attribute will
   be parsed by `qdate` and formatted to a valid value.

 * `max` (Erlang Term) - Assign the `max` value to the textbox (as defined by
   some of the HTML5 Input Types). As with the `value` attribute, if the
   textbox's `type` is set to a date or time format, the `max` attribute will
   be parsed by `qdate` and formatted to a valid value.

 * `step` (integer) - Assign the `min` value to the textbox (as defined by
   some of the HTML5 Input Types).

 * `html_name` (string) - The name attribute of the textbox.

### See Also

 * [base element](./element_base.md)
 * [button element](./button.md)
 * [password element](./password.md)
 * [textarea element](./textarea.md)
 * [checkbox element](./checkbox.md)
 * [range element](./range.md)
 * [dropdown element](./dropdown.md)
 * [option element](./option.md)
 * [jQuery Placeholder Readme](https://github.com/mathiasbynens/jquery-placeholder)
 * [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)
 * [qdate](https://github.com/choptastic/qdate)
