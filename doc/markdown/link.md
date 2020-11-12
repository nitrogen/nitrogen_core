
## Link Element - #link{}

  The link element produces a clickable hyperlink. Depending on its settings,  
  the link can either function as a normal client-side hyperlink, or can
  cause a Nitrogen postback.

### Usage

```erlang
  #link { text="Example Hyperlink", url="http://nitrogenproject.com" },

```

```erlang
   #link { text="Example Postback Link", postback=link_clicked }

```

```erlang
   #link { body=#image { image="path/to/image.gif" }, postback=link_clicked}

```

### Attributes

   * `text` (string) - The text to display.

   * `body` (Nitrogen elements) - Instead of text, specify one or more
     Nitrogen elements (such as an #image) to wrap in a link.

   * `image` (string) - The path to an image to display as the body. Largely
     a shortcut for making icon links.  Serves as a shortcut for the following:

```ERLANG
         #link{url="/edit" body=[
             #image{image="/path/to/some/image.gif"}
         ]}

```

   * `html_encode` (boolean) - Set to true to safely html-encode the link's
     title.

   * `url` (string) - If set, clicking on the link will navigate to this
     URL.

   * `new` (boolean) - Set to true to make link open in a new window.

   * `click` (Action/ or /List of Actions) - Wires the selected actons to
     the \"click\" events.  Due to the commonality of binding actions to the
     click event, this is merely a shortcut for 

```ERLANG
         #link{ text="Do Something", actions=[
            #event{type=click,actions=ListOfActions}
         ]}

```

   * `postback` (Erlang term) - If set, clicking on the link will cause a
     Nitrogen postback with the supplied term.

   * `mobile_target` (Boolean) - If set to false, this will work with
     jQuery Mobile to ensure that it skips using jQuery Mobile's automatic page
     transition loading system.

   * `mobile_dialog` (Boolean) - If set to true, this will work with jQuery mobile to load the target URL in a [jQuery Mobile Dialog](http://jquerymobile.com/demos/1.1.1/docs/pages/page-dialogs.html).

### See Also

 *  [base element](./element_base.md)

 *  [button element](./button.html)

 *  [jQuery Mobile Integration Guide](../jquery_mobile_integration.html)
