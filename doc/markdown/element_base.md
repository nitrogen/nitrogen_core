<!-- dash: Base Element | Guide | ###:Section -->



## Base Element

  In object-oriented parlance, all Nitrogen elements are subclasses of
  the base element. This means that all Nitrogen elements can use the
  attributes listed below.

### Usage

```erlang
   #some_element { id=myElementID, show_if=true, class="someclass", style="border: solid 1px black;" }

```

### Attributes

   * `id` (atom) - The id of the element within the element tree.  The id is
    used to wire events and to reference the element in client side javascript.
    See the Nitrogen wiki for more information.

   * `actions` (action, list of actions) - The actions to attach to the
    element. This can be a single Nitrogen action, or a list of actions.

   * `show_if` (boolean) - If set to true, this element will be rendered.
    Otherwise, it will not.

   * `class` (atom or string) - Set the CSS class of the element.

   * `title` (string) - Set the HTML `title` attribute of the element.

   * `style` (string) - Sets inline CSS style on the element.
  
   * `html_id` (atom or string) - Sets the HTML `id` attribute of the 
    element.

   * `data_fields` (property list) - Sets a series of HTML `data-` fields in
    the element. For example, the following `#panel`:
  
```erlang
  #panel{
    data_fields=[
      {ship, bounty},
      {action, mutiny}
    ]
  }

```
  
    Would produce the following pseudo-HTML:
  
```html
  <div data-ship="bounty" data-action="mutiny"></div>

```
