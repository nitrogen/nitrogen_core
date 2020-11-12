

## Template Element - #template {}


  The template element allows you to specify html chrome for your page (header, footer, columns, etc.)
  while using a special placeholder syntax to call back to your page or any Erlang module.

  To specify a placeholder in the html template file, use the form:
  
```erlang
  [[[Module:Function(Args)]]]

```

  Module can be any module, or can be the atom 'page' to refer to the current
  Nitrogen pagemodule.  The function must return either one or more Nitrogen
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

   * `file` (String) - Path to an html template file, relative
      to the directory from which Erlang was started.

   * `bindings` (Bindings, as expected by erl_eval) - A list of
      variable bindings to be used for arguments specified in the
      template. Example: `[{'MyArg1', "Hello"}, {'MyArg2', "World"}]`

   * `module_aliases` (proplist of {alias, actual_module} pairs) - When 
      `alias` is encountered as the module of a Module:Function(Args) call,
      replace `alias` with `actual_module`.

### See Also

 *  [base element](./element_base.md)

 
