<!-- dash: #restful_upload | Element | ###:Section -->


## RESTful Upload Element - #restful_upload {}

  The button element produces an HTML `<input type=file>` element. The input
  element represents a file-choose dialog.  When the `#restful_submit` element
  from the same `#restful_form` is clicked, the chosen file will be uploaded to
  the server.

  To fetch the content of the uploaded file, you'll have to retrieve the
  uploaded file directly from `SimpleBridge` (as demonstrated below)

### Usage

```erlang
   #restful_upload { iname = upload},

   %% later on return 
   Bridge = wf_context:bridge(),
   UploadedFiles = sbw:post_files(Bridge),
   %% ...
```

### Attributes

   * `html_name` (string) - The name attribute of the `restful_upload` 

### See Also

 *  [Base](./element_base.md)
 *  [Link](./link.md)
 *  [Textbox](./textbox.md)
 *  [Password](./password.md)
 *  [Textarea](./textarea.md)
 *  [Checkbox](./checkbox.md)
 *  [Dropdown](./dropdown.md)
 *  [Dropdown Option](./option.md)
 *  [RESTful Form](./restful_form.md)
 *  [RESTful Reset](./restful_reset.md)
 *  [RESTful Forms Demo](http://nitrogenproject.com/demos/restful)
