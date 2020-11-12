

## HTML5 Footer Element - #html5_footer {}

  Produces the HTML5 Footer element.

### Usage

```erlang
   #article { body=[
      #html5_header{ body=[
         #h1{ text="My Article Title"},
         #time{ pubdate="2013-08-27"}
      ]},
      "Here is some article content",
      #html5_footer{ body="Some footer content"}
   ]}.

```

### Attributes

   * `body` (Nitrogen Elements) - The body of the HTML5 Footer element.

   * `role` (String) - The role of the HTML5 Footer element.

### See Also

 *  [Base element](./element_base.md)

 *  [Article element](article.md)

 *  [HTML5 Header element](html5_header.md)

 *  [Panel element](panel.md)

 *  [About the Footer Element](http://html5doctor.com/the-footer-element/)
