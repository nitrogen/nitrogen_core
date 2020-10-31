

## Article Element - #article {}

  Produces the HTML5 Article element.

### Usage

```erlang
   #article { body=[
      #html5_header{ body="Article Header"},
      "Here is some article content",
      #html5_footer{ body="Some footer content"}
   ]}.

```

### Attributes

   * `body` (Nitrogen Elements) - The body of the article element.

   * `role` (String) - The role of the HTML5 Article element.

### See Also

 *  [Base element](./base.html)

 *  [HTML5 Header element](html5_header.html)
   
 *  [HTML5 Footer element](html5_footer.html)

 *  [Panel element](panel.html)

 *  [About the Article HTML Element](http://html5doctor.com/the-article-element/)
