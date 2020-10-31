

## File Element - #file{}

  Loads the content of the specified file and displays it in the browser.

### Usage

```erlang
   #file { file="/path/to/some/file" }

```

### Attributes

   * `file` (string) - Path to the file to load

   * `include_panel` (boolean) - If `true`, the file contents will be wrapped
      in a Nitrogen `#panel{}` element (which is an HTML `<div>`). If `false`, the
      file contents will merely be sent directly, without any wrapper.

   * `html_encode` (boolean) - Tells whether to safely HTML encode the
      contents of the file or not before presenting.


### See Also

 *  [base element](./base.html)
