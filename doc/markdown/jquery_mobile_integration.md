<!-- dash: jQuery Mobile Integration | Guide | ###:Section -->


## jQuery Mobile Integration

   Integrating jQuery Mobile into Nitrogen is a relatively painless process, but it does require making a few changes, particularly to the template.  For convenience, we provide a default template for mobile, coincidentally called "mobile.html".

### Include the necessary javascript.

   There are two new javascript files that must be included in order properly integrate with jQuery Mobile: jQuery mobile itself (jquery-mobile.js) and the Nitrogen jQuery Mobile script (`nitrogen_jqm.js`), which makes necessary structural changes to the Nitrogen object.

   The `<script>` elements in the head should look something like this. Please note that the order of these includes does matter.

```html
   <script type='text/javascript' src='/nitrogen/jquery.js'></script>
   <script type='text/javascript' src='/nitrogen/jquery-mobile.js'></script>
   <script type='text/javascript' src='/nitrogen/nitrogren.js'></script>
   <script type='text/javascript' src='/nitrogen/nitrogen_jqm.js'></script>

```

### Modify the HTML structure to what jQuery mobile expects

   jQuery mobile does some unique things as far as loading scripts, and providing inter-page transitions. However, in order to take advantage of these functionalities, certain strutural changes must happen with your template.

#### Separate the page into a handful of specific divs.

You want to separate the page into three specific divs with a wrapper div.

The wrapper div must have the attributes `data-role="page"` and `id="pagediv"`

Contained within the Wrapper div are expected three other divs with the data-roles `"header"`, `"content"` and `"footer"`.

This is demonstrated below:

```html
   <body>
      <div data-role="page" id="pagediv">
         <div data-role="header">
            <h1>[[[page:title()]]]</h1>
         </div>
         <div data-role="content">
            [[[page:body()]]]
         </div>
         <div data-role="footer">
            <h4>[[[page:footer()]]]</h4>
         </div>
      </div>
   </body>

```

#### The script tag with jquery mobile version

   The last step in order to ensure the proper loading of javascript from page to page while using transitions, is to add
   `[[[mobile_script]]]`
   to template inside a data-code attribute in the wrapper div.


This is demonstrated below:

```html
   <body>
      <div data-role="page" id="pagediv" data-code="[[[mobile_script]]]">
         <div data-role="header">
            <h1>[[[page:title()]]]</h1>
         </div>
         <div data-role="content">
            [[[page:body()]]]
         </div>
         <div data-role="footer">
            <h4>[[[page:footer()]]]</h4>
         </div>
      </div>
   </body>

```

### The Complete Template

To see the completed jQuery mobile template, check out ./site/templates/mobile.html in your Nitrogen installation, or [see it on github](https://github.com/nitrogen/nitrogen/blob/master/rel/overlay/common/site/templates/mobile.html).

### Dynamically Adding jQuery Mobile Elements in Postbacks

   When adding or updating jQuery mobile elements in Nitrogen postbacks (through the use of `wf:update`, `wf:replace`, etc), you'll want to wire a command that will tell jQuery to scan the page for elements that need rendering.  To do this, somewhere in your postback, make sure to make the following call:

   ```erlang
   wf:defer("$('#pagediv').trigger('create');")
   ```

   This will trigger jQuery mobile to scan the page for changes.  You can see a demo of this on
   [Demos section of the website](http://nitrogenproject.com/demos/mobile_controls2).

   **Note:** This will likely be enhanced so that this is not a required step in the future, but for now, as the Mobile elements are still relatively new, this is a required step.

### See Also

 *  [jQuery Mobile Elements](./jquery_mobile.md)

 *  [jQuery Mobile Homepage](http://jquerymobile.com)
