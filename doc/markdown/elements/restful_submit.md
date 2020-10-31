
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

 *  [Base](./base.html)

 *  [Link](./link.html)

 *  [Textbox](./textbox.html)

 *  [Password](./password.html)

 *  [Textarea](./textarea.html)

 *  [Checkbox](./checkbox.html)

 *  [Dropdown](./dropdown.html)

 *  [Dropdown Option](./option.html)
   
 *  [RESTful Form](restful_form.html)

 *  [RESTful Reset](restful_reset.html)

 *  [RESTful Upload](restful_upload.html)

 *  [RESTful Forms Demo](http://nitrogenproject.com/demos/restful)
