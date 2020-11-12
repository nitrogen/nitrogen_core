

## Panel Element - #panel {}

  The panel element produces an HTML div.

### Usage

```erlang
   #panel { body=[
     #label { text="Label" },
     #value { text="Value" },
     #button { text="Continue", postback=continue }
   ]}

```

### Attributes

   * `body` (Nitrogen element or list of elements.) - Set the element or elements that will be rendered within this panel.

   * `text` (string) - Set to the text of the list item.

   * `html_encode` (boolean) - Set to true to safely html-encode the displayed text.

### See Also

 *  [base element](./element_base.md)

 *  [span element](./span.md)

 
