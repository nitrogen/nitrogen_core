<!-- dash: #template | Element | ###:Section -->



## Template Element - #template {}

  The template element allows you to specify html chrome for your page (header,
  footer, columns, etc.) while using a special callout syntax to call back to
  your page or any Erlang module.

  To specify a callout in the html template file, use the form:

```erlang
  [[[Module:Function(Args)]]]

```

  Module can be any module, or can be the atom 'page' to refer to the current
  Nitrogen page module.  The function must return either one or more Nitrogen
  elements, an Erlang string, or an Erlang binary.

  If the args are identifed like Erlang variables (capitalized first letter)
  such as MyArg1 (as opposed to a static value such as 5), then the template
  element will look in its bindings property for a matching element.

  It's also possible to specify alias module references to other modules. For
  example, the default module alias is actually `[{page, wf:page_module()}]`,
  which basically means "If you encounter page:function(Args), replace `page`
  with the current page's module."

### Usage

```erlang
   #template { files="./priv/templates/myTemplate.html", bindings=[
     {'Binding1', binding1},
     {'Binding2', binding2}
   ]}

```

### Attributes

* `file` (String) - Path to an html template file, relative to the directory
  from which Erlang was started.

* `text` (string or binary) - If present, Nitrogen will use the value as the
  template text, ignoring tthe `file` value (if present).

* `from_type` (atom or string) - What is the file type you're loading (default:
  `html`)

* `to_type` (atom or string) - What is the format we should convert to
  (default: `html`)

* `options` (proplist) - This is a list of options to pass to `pandoc` for
  conversion.

* `callouts` (boolean) - This disables or enables callouts.  If set to `true`,
  the callouts `[[[Module:Function()]]]`` will be parsed.  If `false`, callouts
  will not be parsed or processed.  (Default: `true`)

* `bindings` (Bindings, as expected by `erl_eval`) - A list of variable
  bindings to be used for arguments specified in the template. Example:
  `[{'MyArg1', "Hello"}, {'MyArg2', "World"}]`

* `module_aliases` (proplist of `{alias, actual_module}` pairs) - When `alias`
  is encountered as the module of a `Module:Function(Args)` call, replace
  `alias` with `actual_module`.

### See Also

 *  [base element](./element_base.md)
