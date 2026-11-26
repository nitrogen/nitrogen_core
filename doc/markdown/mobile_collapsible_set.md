<!-- dash: #mobile_collapsible_set | Element | ###:Section -->


## Mobile Collapsible Set Element - #mobile_collapsible_set {}

  This element produces a
  [Mobile Collapsible Set](http://api.jquerymobile.com/collapsible-set/)
  Element, which groups together `#collapsible` elements into a single unified
  structure.  The resulting collapsibles will then close any open collapsibles
  when another within the same group is opened.

### Usage

```erlang
   #mobile_collapsible_set { 
      collapsed=true,
      body=[
         #collapsible{ ... },
         #collapsible{ ... },
         #collapsible{ ... }
      ]
   }

```

### Attributes
   
   * `header_theme` (atom) - Set the jQuery Mobile swatch letter for the contained #collapsible{} elements' header.

   * `content_theme` (atom) - Set the jQuery Mobile swatch letter for the contained #collapsible{} elements' content.
   
   * `mini` (boolean) - Set to true to render the contained #collapsible{} elements in a smaller, more compact form.

### See Also

 *  [jQuery mobile elements](./jquery_mobile.md)

 *  [base element](./element_base.md)

 *  [mobile_collapsible element](./mobile_collapsible.md)

 *  [Mobile Collapsible Demos](http://nitrogenproject.com/demos/mobile_collapsibles)
