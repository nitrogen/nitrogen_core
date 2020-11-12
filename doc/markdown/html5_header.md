

## HTML5 Header Element - #html5_header {}

  Produces the HTML5 Header element.

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

   * `body` (Nitrogen Elements) - The body of the HTML5 Header element.

   * `role` (String) - The role of the HTML5 Header element.

### See Also

 *  [Base element](./element_base.md)

 *  [Article element](./article.html)
   
 *  [HTML5 Footer element](./html5_footer.html)

 *  [Panel element](./panel.html)

 *  [About the Header Element](http://html5doctor.com/the-header-element/)
