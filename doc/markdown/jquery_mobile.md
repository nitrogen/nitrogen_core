<!-- dash: jQuery Mobile | Guide | ###:Section -->


## jQuery Mobile Elements

   All of the jQuery Mobile Elements share a number of attributes in common. Please note that jQuery mobile elements will just be normal HTML elements unless the jQuery mobile javascript has been included. Please see the [jQuery Mobile Integration Guide](./jquery_mobile_integration.md).

### Usage

```erlang
   #some_mobile_element { 
      theme=a,
      data_fields=[
         {icon, minus},
         {shadow, true}
      ]
   }

```

### Attributes

   * `theme` (atom) - Sets the jQuery Mobile Swatch Letter. Can be a-z.

   * `data_fields` ([{field, Attribute},...]) - Sets HTML5 data attributes, which are commonly used for jQuery mobile integration. While most Nitrogen mobile elements already include common data attributes (such as 'data-role', 'data-mini' or 'data-theme', etc), this allows the user to add ones that might not be immediately supported by Nitrogen. See the [jQuery Mobile data attributes](http://jquerymobile.com/test/docs/api/data-attributes.html)

### See Also

 *  [base element](./element_base.md)

 *  [jQuery Mobile Integration Guide](./jquery_mobile_integration.md)

 *  [jQuery Mobile data attributes](http://jquerymobile.com/test/docs/api/data-attributes.html)
