

## Mark Element - #mark {}

  The mark element produces an HTML mark element. The HTML5 mark tag is used
  for highlighting words or prases (such as highlighting search phrases).

### Usage

```erlang
   #panel{body=[
      <<"In this sentence, ">>,
      #mark { text="just this phrase" },
      " will be highlighted"
   ]}.

```

### Attributes

   * `text` (string) - The text to display.

   * `html_encode` (boolean) - Set to true to safely html-encode the
      displayed text.

   * `body` (Nitrogen element or list of elements.) - Set the element or
      elements that will be rendered within this panel.

   * `role` (String) - The role of the HTML5 Mark element.

### See Also

 *  [base element](./element_base.md)

 *  [span element](./span.html)

 *  [About the HTML Mark Element](http://html5doctor.com/draw-attention-with-mark/) 
