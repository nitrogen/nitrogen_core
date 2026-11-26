<!-- dash: #mobile_grid | Element | ###:Element -->


## Mobile Grid Element - #mobile_grid {}

This element produces a [jQuery Mobile Layout Grid](http://jquerymobile.com/demos/1.1.0/docs/content/content-grids.md) Element, which is a wrapper to allow you to break a mobile page up into a grid-like layout, consisting of rows and columns. Each block of this grid is defined with a series of [#mobile_grid_block{}](./mobile_grid_block.md) elements.

### Usage

```erlang
   #mobile_grid { 
      columns=2,
      blocks=[
         #mobile_grid_block{ text="Row 1, Cell 1" },
         #mobile_grid_block{ text="Row 1, Cell 2" },
         #mobile_grid_block{ text="Row 2, Cell 1" },
         #mobile_grid_block{ new_row = true, text="Row 3, Cell 1"},
         #mobile_grid_block{ text="Row 3, Cell 2"}
      ]
   }

```

### Attributes
   
   * `columns` (integer(2-5)) - Set the number of columns to use for this grid, each containing block will automatically be set to the correct row and column, based on this number.

   * `blocks` (List of #mobile_grid_block{}) - Set the list of mobile blocks. To skip to end a row early and skip down to the next row, set the #mobile_grid_block{}'s new_row attribute to 'true'. Elements defined after a 'new_row' element will continue from that  block in a normal fashion.
   
### See Also

 *  [jQuery mobile elements](./jquery_mobile.md)

 *  [base element](./element_base.md)

 *  [mobile\ grid_block element](./mobile_grid_block.md)

 *  [table element](./table.md)
