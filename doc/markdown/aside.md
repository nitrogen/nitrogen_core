

## Aside Element - #aside {}

  Produces the HTML5 Aside element. The aside element can be used as a
  container of information that's not necessarily part of the content, but is
  related to the content, like a glossary of terms, or a list of relevant
  items. We recommend reading the "About the Aside HTML Element" to understand
  its semantics properly.

### Usage

```erlang
   #article { body=[
      "Here is some article content",
      #aside{body=[
         GlossaryOfTerms,
         #br{},
         RelatedArticles
      ]}
   ]}.

```

### Attributes

   * `body` (Nitrogen Elements) - The body of the aside element.

   * `role` (String) - The role of the HTML5 Aside element.

### See Also 

 *  [Base element](./element_base.md.html)

 *  [Article element](article.md)
   
 *  [About the Aside HTML Element](http://html5doctor.com/aside-revisited/)
