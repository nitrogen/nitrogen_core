

## Single Row Element - #singlerow {}

  The single row element is a shortcut for a table with one row.

  Commonly used by CSS anti-purists to get simple layouts to work in cross-browser scenarious.
  See [this site](http://giveupandusetables.com) for more information.

### Usage

```erlang
   #singlerow { cells=[
     #tablecell { body="Cell Text" },
     #tablecell { body=#link { text="Link" }}
   ]}

```

### Attributes

   * `cells` (One or more tablecell elements.) - Set the table cells to be rendered within this row.

### See Also

 *  [base element](./element_base.md)

 *  [tableheader element](./tableheader.md)

 *  [tablecell element](./tablecell.md)

 
