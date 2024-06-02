<!-- dash: #list | Element | ###:Section -->

## List Element - #list {}

The list element produces an HTML list element (`<ol>` and `<ul>`).

### Usage

```erlang
   #list{
      numbered=true,
      body=[
         #listitem{ ... },
         #listitem{ ... },
         #listitem{ ... } 
      ]
   }.
	
```

### Attributes
  
   * `numbered` (boolean) - Set to true to make this a numbered list, otherwise it will be typical bullet list.
 
   * `body` (list of Nitrogen `#listitem{}` elements) - A list of listitems elements

   * `role` (atom or string) - Set the [XHTML Role attribute](http://www.w3.org/TR/xhtml-role/).

### See Also

 *  [base element](./element_base.md)
 *  [listitem element](./listitem.md) 
 *  [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)
