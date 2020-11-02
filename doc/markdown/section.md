

## Section Element - #section {}

  Produces the HTML5 `<section>` element. Note, that the section element is not
  a generate container for anything, that's a `<div>` (which, in Nitrogen is
  represented by a `#panel` element). We strongly recommend reading the "About
  the Section HTML Element" link in the "See Also" section here.

### Usage

```erlang
   #section{ body=[
      #h1{text="Some heading"},
      "Here is some content for this section"
   ]}.

```

### Attributes

   * `body` (Nitrogen Elements) - The body of the section element.

   * `role` (String) - The role of the HTML5 Section element.

### See Also

 *  [Base element](./base.html)

 *  [Article element](article.md)
   
 *  [About the Section HTML Element](http://html5doctor.com/http://html5doctor.com/the-section-element//)
