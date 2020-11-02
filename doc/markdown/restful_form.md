
## RESTful Form Element - #restful_form {}

  The #restful_form{} element produces an HTML form element.
  The form element is necessary if your website has to work without
  javascript. It is _not_ necessary for a pure ajax/javascript driven
  website. 
 
### Usage

```erlang
   #restful_form { 
      method=post|get,
      action="/formdest",
      enctype="multipart/form-data",
      body=FormElements
   }

```

### Attributes

   * `method` (string or atom) - Set the HTTP request-method (typically
                            `post` or `get`).

   * `action` (string) - Set the target-url of the form submit. If
                            left blank, the same page module is used as
                            target.

   * `target` (string or atom) - Set the HTML `target` attribute, which can
                             be used for redirecting the form submission to a 
                             new window, or to a frame. If left blank, will
                             just target the current window.

   * `enctype` (string) - Sets the encoding for the form transmission.

 *  body    - (*string*)  :: Contains the elements of the form


### See Also

 *  [Base](./base.html)

 *  [Link](./link.html)

 *  [Textbox](./textbox.html)

 *  [Password](./password.html)

 *  [Textarea](./textarea.html)

 *  [Checkbox](./checkbox.html)

 *  [Dropdown](./dropdown.html)

 *  [Dropdown Option](./option.html)
   
 *  [RESTful Submit](restful_submit.md)

 *  [RESTful Reset](restful_reset.md)

 *  [RESTful Upload](restful_upload.md)

 *  [RESTful Forms Demo](http://nitrogenproject.com/demos/restful)

