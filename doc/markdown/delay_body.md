<!-- dash: #delay_body | Element | ###:Section -->


## Delay Body Element

The `#delay_body{}` element will render as a placeholder on the page, and once
the page is loaded, initiate a postback to retrieve the contents later.

This is useful in a few scenarios.

* If you have a page with many slow-loading contents (for example, maybe you
  want to live-load 50 stock quotes on one page, each requiring a separate
  external API call).  This will load them and replace them in serial, but not
  hold up rendering the rest of the page.

* If you have user-specific content in a section of the page that is otherwise
  being cached.  This can then allow you to cache and even pre-render content in
  advance, then load  the user-specific stuff later.


This requires an event handler called `delay_body_event(Tag)` in order to work.

### Usage

```erlang
#delay_body{
	placeholder="Loading Username...",
	tag={username, 123}
}
```

#### Event Function

```erlang
delay_body_event({my_content, ID}) ->
	Username = db:get_username(ID),
	#span{text=Username}.
```

### Attributes

* `tag` (/Any Erlang Term/) - This `tag` will be passed to the
  `delay_body_event(Tag)` function to be processed.

* `placeholder` (/String/) - What content to show until the `delay_body_event`
  call returns.

* `delay` (/Integer/) - The number of milliseconds to wait before posting back
  to process the `delay_body_event/1` function

* `method` (/Atom/ - `optimized` or `simple`) - The `optimized` method will return
  faster, requiring fewer postbacks to complete (if you have more than one
  `#delay_body` element on the page).  The `simple` method will use one postback
  per `#delay_event` element, returning a bit slower, however it will be more
  reliable.  The default `method` is `optimized`, however, if the element exists
  in either a function that's being cached, or from a comet processs, the system
  will force the element to use `simple`.  It's rare that you might need to
  manually tweak this, but some situations might call for it if you're
  experiencing.

* `delegate` (/Atom/) - which module will handle the `delay_body_event` call.
  Defaults to the current page module.

### See Also

 * [base element](./element_base.md)
 * [Delay Body Demo](https://nitrogenproject.com/demos/delay_body)
 * [Cache Handler](./cache.md)

