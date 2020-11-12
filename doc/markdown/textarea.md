
## Textarea Element - #textarea {}

  The textarea element produces an HTML textarea.

### Usage

```erlang
   #textarea { text="Some text" }

```

### Attributes

   * `text` (string) - Set the textarea's contents.

   * `rows` (integer) - Set the number of rows in the textarea.

   * `columns` (integer) - Set the number of columns in the textarea.

   * `html_encode` (boolean) - Set to true to safely html-encode the text.

   * `disabled` (boolean) - Set to true to disable this element in the
      browser (sets the HTML `disabled` property, effectly greying out the
      element). Can be dynamically re-enabled with `wf:enable(ElementID)` or
      `wf:wire(ElementID, #enable{})` (Default: `false`)

   * `readonly` (boolean) - Set to true to make this element readonly in the
      browser. Can be toggled with `wf:wire(ElementID, #make_writable{})` and
      `wf:wire(ElementID, #make_readonly{})`.

 *  trap_tabs - (/boolean) :: Set to `true` to allow tabs to be inserted into
     the textarea, rather than moving the focus to the next element in the page.

   * `placeholder` (string) - Sets the placeholder text (text that will be
      in the textarea until the user focuses in the textarea). This uses the
      HTML5 placeholder attribute, which is only supported in newer browsers.
      To add support for older-browsers, you will need to link a placeholder
      script in your template. You can do this by adding the following to your
      template inside the `<head>` section after loading jQuery. 

```html
   <script src='/nitrogen/jquery.placeholder.js' type='text/javascript'></script>

```

   * `html_name` (string) - The name attribute of the textarea.

### See Also

 *  [base element](./element_base.md)

 *  [button element](./button.md)

 *  [textbox element](./textbox.md)

 *  [password element](./password.md)

 *  [checkbox element](./checkbox.md)

 *  [dropdown element](./dropdown.md)

 *  [option element](./option.md)

 *  [jQuery Placeholder Readme](https://github.com/mathiasbynens/jquery-placeholder)
