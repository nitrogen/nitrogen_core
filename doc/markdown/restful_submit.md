
## RESTful Submit Element #restful_submit{}
  The restful_submit element produces an input field of type submit.
  When clicked, the restful_submit field submit the surrounding form.

### Usage

```erlang
   #restful_submit { text="Go!" }

```

### Attributes

   * `text` (string) - The button's text.

   * `body` (Nitrogen element or list of elements) - Instead of using text
    for the button label, this will allow you to embed HTML and Nitrogen
    elements inside the button.

   * `html_encode` (boolean) - Set to true to safely html-encode the text.

   * `html_name` (string) - the name attribute of the restful_submit
    element 

### See Also

 *  [Base](./element_base.md)

 *  [Link](./link.md)

 *  [Textbox](./textbox.md)

 *  [Password](./password.md)

 *  [Textarea](./textarea.md)

 *  [Checkbox](./checkbox.md)

 *  [Dropdown](./dropdown.md)

 *  [Dropdown Option](./option.md)
   
 *  [RESTful Form](restful_form.md)

 *  [RESTful Reset](restful_reset.md)

 *  [RESTful Upload](restful_upload.md)

 *  [RESTful Forms Demo](http://nitrogenproject.com/demos/restful)
