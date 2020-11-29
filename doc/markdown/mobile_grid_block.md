<!-- dash: #mobile_grid_block | Element | ###:Section -->


## Mobile Grid Block Element - #mobile_grid_block {}

This element produces a block element for a container [Mobile Grid](./mobile_grid.md) Element.

### Usage

```erlang
   #mobile_grid { 
      columns=2,
      block=[
         #mobile_grid_block{ text="Row 1, Cell 1" },
         #mobile_grid_block{ text="Row 1, Cell 2" },
         #mobile_grid_block{ text="Row 2, Cell 1" },
         #mobile_grid_block{ new_row = true, text="Row 3, Cell 1"},
         #mobile_grid_block{ text="Row 3, Cell 2"}
      ]
   }

```

### Attributes
  
   * `new_row` (boolean) - Set to true to short-circuit the automatic row/column layout and for the grid to start at the next row down.
 
   * `body` (Nitrogen element or list of elements) - Set the element(s) that will be rendered within this block.

   * `text` (string) - Set to the text of the block, instead of specifying Nitrogen elements.

   * `html_encode` (boolean|whites|encoding fun) - Set to true to have the text safely html encoded (default: 'true'). Set to 'whites' to also encode whitespace. Set to a function with arity 1 to have the contents encoded using that function.
   
### See Also

 *  [jQuery mobile elements](./jquery_mobile.md)

 *  [base element](./element_base.md)

 *  [mobile grid element](./mobile_grid.md)

 *  [table element](./table.md)
