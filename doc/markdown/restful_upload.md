
## RESTful Upload Element - #restful_upload {}
  The button element produces an HTML input element that has type
  attribute 'file'. The input element represents a file-choose dialog.
  When the restful_submit element is clicked, the chosen file will be
  uploaded to the server.
  To fetch the content of the uploaded file, You first have to get the
  request-bridge by calling 'wf_context:request_bridge()'; after
  that you have to query for the uploaded file by searching
  the elements returnd by 'post_files()'.

### Usage

```erlang
   #restful_upload { id = upload, text="Go!" },
   %% later on return 
   Req = wf_context:request_bridge(),
   UploadedFiles = Req:post_files(),
   %% ...

```

### Attributes

   * `text` (string) - The button's title.

   * `html_encode` (boolean) - Set to true to safely html-encode the text.

   * `html_name` (string) - The name attribute of the restful_upload 

### See Also

 *  [Base](./element_base.md)

 *  [Link](./link.html)

 *  [Textbox](./textbox.html)

 *  [Password](./password.html)

 *  [Textarea](./textarea.html)

 *  [Checkbox](./checkbox.html)

 *  [Dropdown](./dropdown.html)

 *  [Dropdown Option](./option.html)

 *  [RESTful Form](restful_form.md)

 *  [RESTful Reset](restful_reset.md)

 *  [RESTful Upload](restful_upload.md)
    
 *  [RESTful Forms Demo](http://nitrogenproject.com/demos/restful)
