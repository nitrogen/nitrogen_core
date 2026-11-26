<!-- dash: #mobile_collapsible | Element | ###:Section -->


## Mobile Collapsible Element - #mobile_collapsible {}

This element produces a [jQuery Mobile Collapsible Content](http://api.jquerymobile.com/collapsible/) Element

### Usage

```erlang
   #mobile_collapsible { 
      collapsed=true,
      header_text="See my favorite movies",
      content_body=[
         #list{body=[
            #listitem{text="Big Trouble in Little China"},
            #listitem{text="Dumb and Dumber"},
            #listitem{text="Gladiator"}
         ]}
      ]
   }

```

### Attributes
   
   * `header_text` (string) - The header text of the collapsible.  This is
     the text that will be clicked on to expand or collapse the content

   * `content_text` (string) - The text content of the element.

   * `content_body` (Nitrogen element or list of elements) - Set the
     element(s) that will be rendered within this panel.

   * `header_theme` (atom) - Set the jQuery Mobile swatch letter for the
     header.

   * `content_theme` (atom) - Set the jQuery Mobile swatch letter for the
     content.
   
   * `collapsed` (boolean) - Set to true to have the element start
     collapsed, false to start open. (default 'true')

   * `mini` (boolean) - Set to true to render the collapsible in a smaller,
     more compact form.

### See Also

 *  [jQuery mobile elements](./jquery_mobile.md)

 *  [base element](./element_base.md)

 *  [mobile\ collapsible\ set element](./mobile_collapsible_set.md)

 *  [panel element](./panel.md)

 *  [Mobile Collapsible Demos](http://nitrogenproject.com/demos/mobile_collapsibles)
