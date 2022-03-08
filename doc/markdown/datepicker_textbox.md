<!-- dash: #datepicker_textbox | Element | ###:Section -->


## Datepicker Textbox Element - #datepicker_textbox {}

  The datepicker textbox element produces an HTML textbox that automatically
  pops up a [jQuery UI Datepicker](http://jqueryui.com/datepicker/) when the
  control takes the focus.

### Usage

```erlang
   #datepicker_textbox{
      id=date1,
      text="5/25/2013",
      options=[
         {dateFormat, "mm/dd/yy"},
         {showButtonPanel, true}
      ]
   }

```

### Attributes

* `text` (string) - Set the textbox's contents.

* `html_encode` (boolean) - Set to true to safely html-encode the text.

* `options` (list of {Key,Value} properties) - Set the jQuery UI options. You can view the full
   list of options at the official [jQuery UI Datepicker API Documentation](http://api.jqueryui.com/datepicker/).
   Here are a few common options:
  * `formatDate` (format string) - Change the format of the displayed
      date (e.g.: `"yy-mm-dd"`, `"mm/dd/yy"` ).
  * `showOtherMonths` (boolean) - Set to true to allow the datepicker to
      show the previous and next month's last and first days in the otherwise
      blank spaces.
  * `selectOtherMonths` (boolean) - If `showOtherMonths` is true, then
      clicking those leading and trailing dates will also put the date in the
      textbox.
  * `numberOfMonths` (integer) - Display the specified number of months
      in the calendar popup rather than just a single month.
  * `changeMonth` (boolean) - Put a "Month" dropdown box in the calendar
      popup
  * `changeYear` (boolean) - Put a "Year" dropdown box in the calendar
      popup
  * `showButtonPanel` (boolean) - Put a button panel below the calendar
      popup which includes a "Today" button, and a "Close" button (the text of
      which [can be customized](http://api.jqueryui.com/datepicker/#option-showButtonPanel)).

* `disabled` (boolean) - Set to true to disable this textbox in the
   browser (sets the HTML `disabled` property, effectly greying out the
   element). Can be dynamically re-enabled with `wf:enable(ElementID)` or
   `wf:wire(ElementID, #enable{})` (Default: `false`)

* `readonly` (boolean) - Set to true to make this element readonly in the
   browser. Can be toggled with `wf:wire(ElementID, #make_writable{})` and
   `wf:wire(ElementID, #make_readonly{})`.

* `placeholder` (string) - Sets the placeholder text (text that will be
   in the textbox until the user focuses in the textbox). This uses the
   HTML5 placeholder attribute, which is only supported in newer browsers.
   To add support for older-browsers, you will need to link a placeholder
   script in your template. You can do this by adding the following to your
   template inside the `<head>` section after loading jQuery. 

```html
   <script src='/nitrogen/jquery.placeholder.js' type='text/javascript'></script>

```

   * `next` (atom) - If set with a valid control ID, pressing the enter key
      in the textbox will automatically move focus to the specified control.

   * `postback` (Erlang term) - If set, pressing the enter key in the
      textbox will automatically initiate a Nitrogen postback with the supplied
      term.

   * `html_name` (string) - The name attribute of the textbox.

### See Also

 *  [base element](./element_base.md)
 *  [button element](./button.md)
 *  [password element](./password.md)
 *  [textarea element](./textarea.md)
 *  [checkbox element](./checkbox.md)
 *  [dropdown element](./dropdown.md)
 *  [option element](./option.md)
 *  [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)
