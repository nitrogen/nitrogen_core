## Open Window Action - `#open_window`

This action opens a new browser window.

### Usage

```erlang
    wf:wire(#open_window {
        url="http://www.example.com",
        name="exampleWindow",
        height=600,
        width=800,
        left=200,
        top=200,
        menubar=true,
        statusbar=false,
        titlebar=true,
        options="resizable,scrollbars,status"
    }).
```

### Attributes

  * `url` (String) - URL of the page to load in the window.
  * `name` (String) - The name of the window.
  * `height` (Integer) - The height of the window.
  * `width` (Integer) - The width of the window.
  * `left` (Integer) - The left position of the window on the screen.
  * `top` (Integer) - The top position of the window on the screen.
  * `menubar` (Boolean) - Show or hide the menubar. 
  * `statusbar` (Boolean) - Show or hide the statusbar.
  * `titlebar` (Boolean) - Show or hide the titlebar.
  * `options` (Proplist) - A proplist of other options such as `{resizable,
	"yes"}`. See [Window.open() documentation](https://developer.mozilla.org/en-US/docs/Web/API/Window/open)
	for possible values.


### See Also

 * [base action](./action_base.md)
 * [Window.open() documentation](https://developer.mozilla.org/en-US/docs/Web/API/Window/open)
