
## Mobile List Element - #mobile_list {}

This element produces a wrapper for [jQuery Mobile Listviews](http://jquerymobile.com/test/docs/lists/docs-lists.html).

### Usage

```erlang
   #mobile_list { 
      theme=d,
      body=[
         #mobile_listitem { ... },
         #mobile_listitem { ... },
         #mobile_listitem { ... }
      ]
   }

```

### Attributes
 
   * `inset` (boolean) - Whether or not to render the list inset or outset.

   * `body` (string) - The body of the list element, generally containing only `#mobile_list{}` elements.

### See Also

 *  [jQuery mobile elements](./jquery_mobile.html)

 *  [base element](./base.html)

 *  [label element](./mobile_listitem.html)

 *  [list element](./list.html)

 *  [Mobile List Demos](http://nitrogenproject.com/demos/mobile_list)
