<!-- dash: #basic_element | Element | ##:Section -->

# Basic Elements

The following basic elements are defined in Nitrogen. The basic elements all
have the same attributes and in all cases (except `#panel{}`), render as their
directly named HTML element.  For example `#span{}` becomes the HTML `<span>`.

## h1 element - `#h1{}`

Produces an HTML `<H1>`

## h2 element - `#h2{}`

Produces an HTML `<H2>`

## h3 element - `#h3{}`

Produces an HTML `<H3>`

## h4 element - `#h4{}`

Produces an HTML `<H4>`

## h5 element - `#h5{}`

Produces an HTML `<H5>`

## h6 element - `#h6{}`

Produces an HTML `<H6>`

## p element - `#p{}`

Produces an HTML `<P>`

## span element - `#span{}`

Produces an HTML `<SPAN>`

## em element - `#em{}`

Produces an HTML `<EM>`

## i element - `#i{}`

Produces an HTML `<I>`

## pre element - `#pre{}`

Produces an HTML `<PRE>`

## code element - `#code{}`

Produces an HTML `<CODE>`

## strong element - `#strong`

Produces an HTML `<STRONG>`

## panel element - `#panel{}`

The panel element is the odd basic element and produces an HTML `<DIV>`.  This
is because `div` is a keyword in Erlang, so `#panel{}` is used.

### Usage

```erlang
   #panel { body=[
     #label { text="Label" },
     #value { text="Value" },
     #button { text="Continue", postback=continue }
   ]}

```

## header element - `#header{}`

Produces an HTML `<HEADER>`

## html5\_header element - `#html5_header{}`

Alias to `#header{}`. Deprecated in Nitrogen 3.0, but currently kept for backwards compatibility

## footer element - `#footer{}`

Produces an HTML `<FOOTER>`

## html5\_footer element - `#html5_footer{}`

Alias to `#footer{}`. Deprecated in Nitrogen 3.0, but currently kept for backwards compatibility

## nav element - `#nav{}`

Produces an HTML `<NAV>`

## main element - `#main{}`

Produces an HTML `<MAIN>`

## section element - `#section{}`

Produces an HTML `<SECTION>`

## article element - `#article{}`

Produces an HTML `<ARTICLE>`

## aside element - `#aside{}`

Produces an HTML `<ASIDE>`

## summary element - `#summary{}`

Produces an HTML `<SUMMARY>`

## mark element - `#mark{}`

Produces an HTML `<MARK>`



### Attributes

   * `body` (Nitrogen element or list of elements) - Set the element or elements that will be rendered within this panel.

   * `text` (string) - Set to the text of the list item.

   * `html_encode` (boolean) - Set to true to safely html-encode the displayed text.

### See Also

 * [base element](./element_base.md)
 * [HTML Semantic Elements](https://developer.mozilla.org/en-US/docs/Glossary/Semantics#semantic_elements)
