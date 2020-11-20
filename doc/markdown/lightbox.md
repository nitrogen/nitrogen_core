<!-- dash: #lightbox | Element | ###:Section -->



## Lightbox Element - #lightbox {}

  The lightbox element provides the basics for a Web 2.0 lightbox. 

  To use, create a lightbox element on your Nitrogen page, and set
  the body attribute.

  When the lightbox is visible, an overlay is placed over the current webpage,
  and the elements specified in the body are placed in the center of the browser.

### Usage

```erlang
   #lightbox { id=lightbox1, body=[
     #panel { class=myPanel, body=[
       #h1 { text="Title" },
       "Some body text.",
       #button { text="Close" }
     ]}
   ]}

```

### Attributes

   * `body` (Nitrogen element or list of elements.) - Set the element or elements that will be rendered in the lightbox.

### See Also

 *  [base element](./element_base.md)

 
