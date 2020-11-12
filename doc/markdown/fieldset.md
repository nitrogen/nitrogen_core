

## Fieldset Element - #fieldset {}

  The fieldset element produces an HTML fieldset element containing the expected legend 
  element. This is used for grouping related elements together in a coherent way.

### Usage

```erlang
   #fieldset { legend_text="This Section", body=[ #span{text="The body of the fieldset"} ] }

```

### Attributes

   * `legend_text` (string) - The legend (or title) of the fieldset element

   * `legend_html_encode` (boolean) - Set to true to safely html-encode the displayed text

   * `legend_body` (Elements) - The body of the legend in Nitrogen terms and raw HTML.

   * `text` (string) - The body of the fieldset in a text format.

   * `body` (Elements) - The body of the fieldset in Nitrogen terms and raw HTML.

   * `html_encode` (boolean) - Set to true to safely html-encode the displayed text body.

### See Also

 *  [base element](./element_base.md)

