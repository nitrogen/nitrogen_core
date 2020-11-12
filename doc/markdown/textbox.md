
## Textbox Element - #textbox {}

  The textbox element produces an HTML textbox.

### Usage

```erlang
   #textbox { id=textbox1, text="Some text.", next=textbox2 }

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
      HTML5 placeholder attribute, which is only supported in newer browsers.
      To add support for older-browsers, you will need to link a placeholder
      script in your template. You can do this by adding the following to your
      template inside the `<head>` section after loading jQuery. 

```html
   <script src='/nitrogen/jquery.placeholder.js' type='text/javascript'></script>

```

   * `type` (atom|string) - Sets the input box "type" attribute. This is
      especially useful for taking advantage of the newer HTML5 values for
      this. It can be any of the following: `search`, `email`, `range`, `url`,
      `tel`, `number`, `date`, `month`, `week`, `time`, `datetime`,
      `datetime-local`, `color`, as well as the original HTML attributes,
      `text`, `button`, `submit`, `image`, and `hidden`.  (Default: `text`)

      [About HTML5 Input Types](http://html5doctor.com/html5-forms-input-types/)

   * `next` (atom) - 
      If set with a valid control ID, pressing the enter key in the 
      textbox will automatically move focus to the specified control.

   * `postback` (Erlang term) - 
      If set, pressing the enter key in the textbox will automatically
      initiate a Nitrogen postback with the supplied term.

   * `html_name` (string) - The name attribute of the textbox.

### See Also

 *  [base element](./element_base.md)

 *  [button element](./button.html)

 *  [password element](./password.html)

 *  [textarea element](./textarea.html)

 *  [checkbox element](./checkbox.html)

 *  [dropdown element](./dropdown.html)

 *  [option element](./option.html)

 *  [jQuery Placeholder Readme](https://github.com/mathiasbynens/jquery-placeholder)

	*  [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)
 
