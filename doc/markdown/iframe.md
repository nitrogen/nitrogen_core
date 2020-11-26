<!-- dash: #iframe | Element | ###:Section -->



## Iframe Element - #iframe {}

   The iframe element produces an HTML iframe. (an HTML `<iframe>` tag).

### Usage

```erlang
   #iframe {
       src = "//www.youtube.com/embed/AAF58aJSr28?rel=0",
       name = "homeVideo",
       allowfullscreen = "",
       html_id = "homeVideo",
       frameborder = "0"
   }

```
   
### Attributes

   * `src` (string) - Set the HTML `src` attribute of the iframe. This can
    be either an absolute URL (`http://hostname.com/path/to/src`) or a
    relative URL (`/path/to/src`).

   * `srcdoc` (string) - Set the HTML `srcdoc` attribute of the iframe.

   * `height` (integer) - Set the HTML `height` attribute of the iframe.

   * `width` (integer) - Set the HTML `width` attribute of the iframe.

   * `align` (Atom, 'left', 'top', 'right', 'middle', or 'bottom') - Set the alignment of an iframe relative to surrounding elements.

   * `frameborder` (integer, 0 or 1) - Set the HTML `frameborder` attribute of the iframe.  

   * `name` (string) - Set the HTML `name` attribute of the iframe.

   * `sandbox` (string) - Set the HTML `sandbox` attribute of the iframe.

   * `seamless` (string) - Set the HTML `seamless` attribute of the iframe.

   * `allowfullscreen` (boolean) - Set the HTML `allowfullscreen` attribute of the iframe.
       
### See Also

 *  [base element](./element_base.md)

 *  [About the iframe Element](http://html5doctor.com/element-index/#iframe)

 *  [W3C iframe reference spec](http://www.w3.org/html/wg/drafts/html/master/embedded-content.html#the-iframe-element)
