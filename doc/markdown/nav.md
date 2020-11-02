

## Nav Element - #nav {}

  Produces the HTML5 Nav element.

### Usage

```erlang
   #nav { class=main_navigation, body=[
      #link{text="Menu item 1", url="/item1"},
      #link{text="Menu item 2", url="/item2"},
      ...
   ]}.

```

### Attributes

   * `body` (Nitrogen Element or Elements) - The body of the nav element.

   * `role` (String) - The role of the HTML5 Footer element.

### See Also

 *  [Base element](./base.html)

 *  [About the Nav HTML5 Element](http://html5doctor.com/nav-element/)
