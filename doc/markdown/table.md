<!-- dash: #table | Element | ###:Section -->



## Table Element - #table {}

The table element allows you to build an HTML table in Nitrogen.

### Usage

```erlang

   #table { rows=[
     #tablerow { cells=[
       #tableheader { text="Name" },
       #tableheader { text="Location" }
     ]},
     #tablerow { cells=[
       #tablecell { text="Rusty" },
       #tablecell { text="USA" }
     ]},
     #tablerow { cells=[
       #tablecell { text="Jon" },
       #tablecell { text="Iceland" }
     ]},
     #tablerow { cells=[
       #tablecell { text="Martin" },
       #tablecell { text="Germany" }
     ]}	
   ]}

```

### Attributes

* `rows` (One or more [tablerow](./tablerow.md) elements.) - Set the rows to
  be rendered within the table element.

### See Also

 *  [base element](./element_base.md)
 *  [tablerow element](./tablerow.md)
 *  [tableheader element](./tableheader.md)
 *  [tablecell element](./tablecell.md)
 *  [singlerow element](./singlerow.md)
