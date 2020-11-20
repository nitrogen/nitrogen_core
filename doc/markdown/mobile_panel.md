<!-- dash: #mobile_panel | Element | ###:Section -->


## Mobile Panel Element - #mobile_panel {}

  This element produces a
  [jQuery Mobile Panel](http://view.jquerymobile.com/1.3.1/dist/demos/widgets/panels/)
  Element, which is typically used for making mobile-style slide-out menus.

### Usage

```erlang
  #mobile_panel { 
    display_mode=push,
    body=[
      #h3{text="Menu"},
      "Here is a slide-out side menu"
    ]
  }.

```

### Attributes
   
   * `body` (Nitrogen element or list of elements) - Set the element(s) that
    will be rendered within this panel.

   * `theme` (atom) - Set the jQuery Mobile swatch letter for the content.
   
   * `dismissible` (boolean) - Set to true to allow it to be dismissed with
    the Escape key or clicking outside of the panel contents.

   * `position` (left or right) - Tells which side of the screen to place
    the panel. (Default: `left`)

   * `display_mode` (reveal, push, or overlay) - Which way to animate the
    panel into visibility. (Default: `reveal`)

### See Also

 *  [jQuery mobile elements](./jquery_mobile.md)

 *  [base element](./element_base.md)

 *  [toggle_mobile_panel action](toggle_mobile_panel.md)

 *  [Mobile Panel Demonstration](http://nitrogenproject.com/demos/mobile_panel)
