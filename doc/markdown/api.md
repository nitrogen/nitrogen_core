<!-- dash: Function Reference | Guide | ##:Section -->

# Nitrogen Function Reference/API

<!-- start_contents -->
<!-- end_contents -->

## Live Page Updates

  TargetID can refer to the name of an element, or it can be a JQuery
  selector path. See [Element Paths](paths.md) for more information.

  For every one of these functions which offer a `Priority` argument,
  `Priority` can be either the atom `eager`, `normal`, or `defer`. `eager` will
  always execute before `normal`, which will always execute before `defer`.

  Any wiring which does not explicitly specify a priority will wire at `normal`
  priority.

* <a name="wf_set"></a>`wf:set(TargetID, Value) -> ok`
* <a name="wf_set"></a>`wf:set(Priority, TargetID, Value) -> ok`

   Update a form element (textbox, dropdown, checkbox, etc) to set text value
   of TargetID. Can also be used to set the `src` attribute of an image tag.

* <a name="wf_enable"></a>`wf:enable(Target) -> ok`
* <a name="wf_enable"></a>`wf:enable(Priority, Target) -> ok`

   Enables the target form field or button.

* <a name="wf_disable"></a>`wf:disable(Target) -> ok`
* <a name="wf_disable"></a>`wf:disable(Priority, Target) -> ok`

   Disables the target form field or button.

* <a name="wf_update"></a>`wf:update(TargetID, Elements) -> ok`
* <a name="wf_update"></a>`wf:update(Priority, TargetID, Elements) -> ok`

   Update the contents of TargetID with a new set of Nitrogen Elements.

* <a name="wf_replace"></a>`wf:replace(TargetID, Elements) -> ok`
* <a name="wf_replace"></a>`wf:replace(Priority, TargetID, Elements) -> ok`

   Replace TargetID with a new set of Nitrogen Elements.

* <a name="wf_remove"></a>`wf:remove(TargetID) -> ok`
* <a name="wf_remove"></a>`wf:remove(Priority, TargetID) -> ok`

   Remove TargetID from the DOM.

* <a name="wf_insert_top"></a>`wf:insert_top(TargetID, Elements) -> ok`
* <a name="wf_insert_top"></a>`wf:insert_top(Priority, TargetID, Elements) -> ok`

   Insert Nitrogen Elements at the top of TargetID (within the contents of
   TargetID), shifting the existing contents downward.

* <a name="wf_insert_bottom"></a>`wf:insert_bottom(TargetID, Elements) -> ok`
* <a name="wf_insert_bottom"></a>`wf:insert_bottom(Priority, TargetID, Elements) -> ok`

   Insert Nitrogen Elements at the bottom of the TargetID (within the contents
   of TargetID), below the existing contents.

* <a name="wf_insert_before"></a>`wf:insert_before(TargetID, Elements) -> ok`
* <a name="wf_insert_before"></a>`wf:insert_before(Priority, TargetID, Elements) -> ok`

   Insert Nitrogen Elements before TargetID in the DOM, shifting the existing
   contents downward.

* <a name="wf_insert_after"></a>`wf:insert_after(TargetID, Elements) -> ok`
* <a name="wf_insert_after"></a>`wf:insert_after(Priority, TargetID, Elements) -> ok`

   Insert Nitrogen Elements after TargetID in the DOM, below the existing
   contents.

* <a name="wf_flash"></a>`wf:flash(Elements) -> ok`

   Insert the Nitrogen Elements as a new flash message.

## Convenience Functions

* <a name="wf_f"></a>`wf:f(Format, Data) -> String or Binary`

   Convenience function to format a string similar to
   `io_lib:format(Format, Data).` Returns a flattened list.

   Note, if `Format` is a binary, the return type will be a binary, and if
   `Format` is a string, the return type will be a string.

* <a name="wf_coalesce"></a>`wf:coalesce([List]) -> Item`

   Return the first element in the list that is not 'undefined'.

* <a name="wf_is_string"></a>`wf:is_string(Term) -> Bool`

   Return true if the Term is an Erlang string. That is, a flat list
   of integers.

* <a name="wf_to_list"></a>`wf:to_list(Term) -> List`

   Convert the supplied term to a flat list, if possible. Useful for
   turning Integers, Atoms, Binaries into Strings.

* <a name="wf_to_atom"></a>`wf:to_atom(Term) -> Atom`

   Convert the supplied term into an Atom, if possible. Useful for
   turning Integers, Binaries, and Strings into Atoms.

* <a name="wf_to_existing_atom"></a>`wf:to_existing_atom(Term) -> Atom`

   Convert the supplied term to an Atom that already exists in the Erlang VM.
   Same as `wf:to_atom/1` but safer when in doubt. (see
   [the advanced section of the Erlang docs pertaining to atoms](http://www.erlang.org/doc/efficiency_guide/advanced.html)).

* <a name="wf_to_binary"></a>`wf:to_binary(Term) -> Binary`

   Convert the supplied term into a Binary, if possible. Useful for
   turning Integers, Atoms, and Strings into Binaries.

* <a name="wf_to_integer"></a>`wf:to_integer(Term) -> Integer`

   Convert the supplied term into an Integer, if possible. Useful for turning
   Atoms, Strings, and Binaries into Integers.

* <a name="wf_html_encode"></a>`wf:html_encode(String) -> EncodedString`

   HTML encode the supplied String, converting things like < and > into &lt;
   and &gt;.

* <a name="wf_html_decode"></a>`wf:html_decode(String) -> DecodedString`

   HTML decoding decode the supplied String, converting things like &lt; and
   &gt; into < and >.

* <a name="wf_url_encode"></a>`wf:url_encode(String) -> EncodedString`

   URL encode the supplied String, converting potentially URL-breaking
   characters into percent notation (%XX).

* <a name="wf_url_decode"></a>`wf:url_decode(String) -> DecodedString`

   URL decode the supplied String, converting a percent-encoded String into a
   normal String.

* <a name="wf_to_qs"></a>`wf:to_qs(PropList) -> EncodedQueryString`

   Encode a PropList (a list of `{Key, Value}` tuples) into a URL-encoded query
   string (returned as an IOList).

* <a name="wf_hex_encode"></a>`wf:hex_encode(String) -> EncodedString.`

   Hex-encode the supplied String.

* <a name="wf_hex_decode"></a>`wf:hex_decode(String) -> DecodedString`

   Convert a hex-encoded String to a normal String.

* <a name="wf_temp_id"></a>`wf:temp_id() -> String`

   Return a temp id. Useful for naming an Element so that you can
   refer to it during a postback later, without giving it a specific
   name.

* <a name="wf_js_escape"></a>`wf:js_escape(String) -> EscapedString`

   Convert a String to a JS-safe string by adding backslashes to quotes and newlines.

* <a name="wf_json_encode"></a>`wf:json_encode(Data) -> JsonEncodedString`

   Convert a data structure (Proplist, list, etc) to a JSON encoded string.

* <a name="wf_json_decode"></a>`wf:json_decode(JsonEncodedString) ->`

   Convert a JSON-encoded string back to an Erlang data structure.

* <a name="wf_join"></a>`wf:join([Terms],Delimiter) -> [Terms]`

  This will join the `Terms` on the delimiter, regardless of the type of
  `Delimiter`.  This function exists because Erlang didn't provide a means to
  join a list in a non-string fashion before Erlang 19.0.  

```erlang
   wf:join([Line1,Line2,Line2],#br{}).

```

## Event Wiring

* <a name="wf_wire"></a>`wf:wire(Actions) -> ok`

   Wire actions to the page. The Actions are applied against the entire page unless a
   trigger or target are specified within the action itself.

   For example, show a Javascript alert:

```erlang
   wf:wire(#alert { text="Hello, World!" })

```

* <a name="wf_wire"></a>`wf:wire(TargetID, Actions) -> ok`

   Wire actions to the page, targeted against supplied TargetID.
   For example, hide a Panel:

```erlang
   wf:wire(PanelID, #hide {})

```

* <a name="wf_wire"></a>`wf:wire(TriggerID, TargetID, Actions) -> ok`

   Wire actions to the page, triggering on the supplied TriggerID and targeting
   against the supplied TargetID. This allows you to wire actions (such as
   #event) that listen to a click on one element and modify a different
   element.

   For example, when a button is clicked, hide a panel:

```erlang
   wf:wire(ButtonID, PanelID, #event { type=click, actions=#hide {} })

```

* <a name="wf_eager"></a>`wf:eager(Actions) -> ok`
* <a name="wf_eager"></a>`wf:eager(TargetID, Actions) -> ok`
* <a name="wf_eager"></a>`wf:eager(TriggerID, TargetID, Actions) -> ok`

   Wire actions to the page, however, execute the actions before any
   normal or deferred priority wired actions.

* <a name="wf_defer"></a>`wf:defer(Actions) -> ok`
* <a name="wf_defer"></a>`wf:defer(TargetID, Actions) -> ok`
* <a name="wf_defer"></a>`wf:defer(TriggerID, TargetID, Actions) -> ok`

   Wire actions to the page, however, execute these actions after any
   normal or eager priority wired actions.

* <a name="wf_continue"></a>`wf:continue(Tag, Function, IntervalInMS, TimeoutInMS) -> ok`

   Spawn the provided function (arity 0) and tell the browser to poll for the
   results at the specified interval, with a timeout setting.  so See
   [continuations example](http://nitrogenproject.com/demos/continuations) for
   usage.

## Asynchronous Page Updates (Comet, Continuations)

* <a name="wf_comet"></a>`wf:comet(Function) -> {ok, Pid}`

   Spawn a comet function, and tell the browser to open a COMET request to
   receive the results in real time.  See\
   [example 1](http://nitrogenproject.com/demos/comet1),
   [example 2](http://nitrogenproject.com/demos/comet2), and 
   [example 3](http://nitrogenproject.com/demos/comet3) for usage.

* <a name="wf_comet"></a>`wf:comet(Function, LocalPool) -> {ok, Pid}`

   Spawn a function connected to the specified local pool.

* <a name="wf_comet_global"></a>`wf:comet_global(Function, GlobalPool) -> {ok, Pid}`

   Spawn a function connected to the specified global pool.

* <a name="wf_send"></a>`wf:send(LocalPool, Message)`

   Send the specified message to all comet functions connected to the
   specified local pool.

* <a name="wf_send_global"></a>`wf:send_global(GlobalPool, Message)`

   Send the specified message to all comet function connected to the
   specified GlobalPool.

* <a name="wf_flush"></a>`wf:flush() -> ok`

   Normally, the results of a comet function are sent to the browser when the
   function exits.  `flush/0` pushes results to the browser immediately, useful
   for a looping comet function.

* <a name="wf_async_mode"></a>`wf:async_mode() -> comet | {poll, Interval}`

   Return the current async mode, either `comet` or `{poll, IntervalInMS}`.

* <a name="wf_async_mode"></a>`wf:async_mode(Mode)`

   Set the current async mode, either `comet` or `{poll, IntervalInMS}`.

* <a name="wf_switch_to_comet"></a>`wf:switch_to_comet()`

   Run all current and future async processes in comet mode. This uses more
   resources on the server, as HTTP connections stay open.

* <a name="wf_switch_to_polling"></a>`wf:switch_to_polling(IntervalInMS)`

   Run all current and future async processes in polling mode. This
   uses more resources on the client, as the application must issue a
   request every `IntervalInMS` milliseconds.

* <a name="wf_continue"></a>`wf:continue(Tag, Function) -> ok`

   Spawn the provided function (arity 0) and tell the browser to poll for the
   results.  See [continuations example](http://nitrogenproject.com/demos/continuations)
   for usage.

* <a name="wf_continue"></a>`wf:continue(Tag, Function, Interval) -> ok`

   Spawn the provided function (arity 0) and tell the browser to poll for the
   results at the specified interval.  See
   [continuations example](http://nitrogenproject.com/demos/continuations) for
   usage.

### Comet notes

  The comets functionality considers some internal messages other than the ones
  delivered by `wf:send/2` and `wf:send_global/2`. These messages are:

  + `'INIT'`
  
    The init message is sent to the first process in a comet pool.
  
  + `{'JOIN', Pid}`
  
    This message is sent to already existing comets when a new process joins to
    the pool
  
  + `{'LEAVE', Pid}`
  
    This message is triggered when certain comet process terminates and it is
    delivered to all other processes in the pool
  
    Optionally you can detect when the user leaves the page with a comet by
    trapping its exit signal.

```erlang
comet_function() ->
  process_flag(trap_exit, true),
  receive
    {'EXIT', _, Message} ->
		?PRINT(Message),
		io:format("The user has left the page.~n")
  end.
```

## Caching

* <a name="wf_cache"></a>`wf:cache(Key, TTL, Fun) -> Value`

  Allow cache or return the results for the function `Fun` (This function
  should be a 0-arity function). The `Key` argument is used to identify the
  cached item, and should uniquely identify the data that is being cached.  The
  `TTL` argument is used to specify the time-to-live in milliseconds for the
  cached item; after the TTL period has expired, the cached data is removed.

* <a name="wf_cache"></a>`wf:cache(Key, Fun) -> Value`
  
  This is the same as calling `wf:cache(Key, infinity, Fun)`. Basically just
  caching a result with no expiration date.

* <a name="wf_cache"></a>`wf:cache(Key) -> Value`

  This is the same as calling `wf:cache(Key, infinity, fun() -> undefined end)`.
  Basically just return a prestored cache result, and if there is none,
  store `undefined` and return `undefined`.

* <a name="wf_set_cache"></a>`wf:set_cache(Key, TTL, Value) -> ok`
* <a name="wf_set_cache"></a>`wf:set_cache(Key, Value) -> ok`

  This will stored the provided `Value` in the cache with `Key` as the
  identifier.  Unlike `wf:cache/[1,2,3]`, this will not execute the provided
  value.  If `Value` is a function, then the cache will store the actual
  function.

* <a name="wf_clear_cache"></a>`wf:clear_cache(Key) -> ok`

  Delete the cached item with the `Key` (if there is one)

* <a name="wf_clear_all_cache"></a>`wf:clear_all_cache() -> ok`

  Completely clear the cache

* <a name="wf_cache_rendered"></a>`wf:cache_rendered(Key, TTL, Elements) -> {ok, HTML}`

  This is a special cache function to pre-render the `Elements` argument.
  `Elements` can be either some Nitrogen elements to be rendered, or it can be
  an arity-0 function that will be executed, and the contents of which will be
  cached.  Further, any actions that are wired  to the client will be
  evaluated, captured, and returned appropriately.

  The return value for this is `{ok, HTML}` with the side effect that any of
  the captured actions will be wired accordingly.

## Redirect

* <a name="wf_redirect"></a>`wf:redirect(URL) -> ok`

   Redirect to the provided `URL`.

* <a name="wf_redirect_to_login"></a>`wf:redirect_to_login(URL) -> ok`

  See Below.

* <a name="wf_redirect_to_login"></a>`wf:redirect_to_login(URL, PostLoginURL) -> ok`

   Redirect to the provided URL, attaching a token on the end. The receiving
   page can subsequently call `wf:redirect_from_login(DefaultURL)` to send
   the user to `PostLoginURL`.

   If `PostLoginURL` is not provided, it will default to the current page's
   URL.

* <a name="wf_redirect_from_login"></a>`wf:redirect_from_login(DefaultURL) -> ok`

   Redirect the user back to a page that called `wf:redirect_to_login(URL)` or
   to the `PostLoginURL` provided to `wf:redirect_to_login(URL, PostLoginURL)`
   If the user came to the page for some other reason, then the user is
   redirected to the provided `DefaultURL`.


## Session State

* <a name="wf_session"></a>`wf:session(Key) -> Value or 'undefined'`

   Retrieve the session value stored under the specified key.
   For example, retrieve the value of 'count' for the current user:
   `Count ` wf:session(count)=

* <a name="wf_session_default"></a>`wf:session_default(Key, DefaultValue) -> Value.`

   Retrieve the session value stored under a specific key. If not
   found, return the supplied default value.

* <a name="wf_session"></a>`wf:session(Key, Value) -> OldValue`

   Store a session variable for the current user. Key and Value can be any Erlang term.
   For example, store a count: `wf:session(count, Count)`

   Returns the previous value associated with Key.


* <a name="wf_clear_session"></a>`wf:clear_session() -> ok`

   Clear the current user's session.

* <a name="wf_logout"></a>`wf:logout() -> ok`

   Clear session state, page state, identity, and roles.

## Page State

* <a name="wf_state"></a>`wf:state(Key) -> Value`

   Retrieve a page state value stored under the specified key. Page State is
   different from Session State in that Page State is scoped to a series
   of requests by one user to one Nitrogen Page.

* <a name="wf_state_default"></a>`wf:state_default(Key, DefaultValue) -> Value.`

   Retrieve a page state value stored under the specified key. If the
   value is not set, then return the supplied default value.

* <a name="wf_state"></a>`wf:state(Key, Value) -> ok`

   Store a page state variable for the current user. Page State is
   different from Session State in that Page State is scoped to a series
   of requests by one user to one Nitrogen Page.

* <a name="wf_clear_state"></a>`wf:clear_state() -> ok`

   Clear a user's page state.

## Authentication and Authorization

* <a name="wf_user"></a>`wf:user() -> User or 'undefined'`

   Return the user value that was previously set by `wf:user(User)`

* <a name="wf_user"></a>`wf:user(User) -> ok`

   Set the user for the current session.

* <a name="wf_clear_user"></a>`wf:clear_user() -> ok`

   Same as `wf:user(undefined)`.

* <a name="wf_role"></a>`wf:role(Role) -> boolean`

   Check if the current user has a specified role.

* <a name="wf_role"></a>`wf:role(Role, IsInRole) -> ok`

   Set whether the current user is in a specified role. `IsInRole` should be a
   boolean (`true` or `false`)

* <a name="wf_clear_roles"></a>`wf:clear_roles() -> ok`

   Remove the user from all roles.

## Web Request and Response

* <a name="wf_in_request"></a>`wf:in_request() -> boolean()`

   Checks if the current running process is actually inside a Nitrogen request,
   that is to say, that it has a Nitrogen-initialized context.  Returns `true`
   if the current process is a Nitrogen request, and `false` otherwise.

   This will return `true` in initial requests, postback requests, and comet
   processes.

* <a name="wf_q"></a>`wf:q(AtomKey) -> String | undefined`

   Get all query string and POST values for the provided key. If more than one
   AtomKey matches, then this will throw an error, use `wf:qs(AtomKey)`
   instead. Returns the atom `undefined` if there's no matching value.

   **Mnemonic:** Think `q` as "Query".

* <a name="wf_mq"></a>`wf:mq(ListOfAtomKeys) -> [ListOfStrings]`

   Get the list of query string and POST values for the provided keys, and
   return the list of values for the keys. Syntactical sugar equivilant of:

```erlang
   [wf:q(AtomKey) || AtomKey <- ListOfAtomKeys]

```

   **Mnemonic**: Think `mq` as "Multi Query".

* <a name="wf_q_pl"></a>`wf:q_pl(ListOfAtomKeys) -> [{Key,Values},...]`

   Takes a list of keys returns a proplist of keys and respective values from
   query string and POST values.

   **Mnemonic**: Think `q_pl` as "Query Proplist"

Example:
```erlang
  wf:q_pl([favorite_robot,favorite_dinosaur,favorite_hobbit])

```

  Returns something like:
```erlang
  [
      {favorite_robot,"Optimus Prime"},
      {favorite_dinosaur,"Velociraptor"},
      {favorite_hobbit,"Samwise"}
  ].


```


* <a name="wf_qs"></a>`wf:qs(AtomKey) -> [String]`

   Get a list of query string and POST values for the provided
   key. (This acts like `wf:q(AtomKey)` in Nitrogen 1.0.)

   **Mnemonic:** Think `qs` as "Query Plural"

* <a name="wf_mqs"></a>`wf:mqs(ListOfAtomKeys) -> [ListOfStrings]`

   Get a list of query string and POST values for the provided list of keys.
   Syntactical sugar equivilant of:

```erlang
   [wf:qs(AtomKey) || AtomKey <- ListOfAtomKeys]

```

   **Mnemonic**: Think `mqs` as "Multi Query Plural"

* <a name="wf_qs_pl"></a>`wf:qs_pl(ListOfAtomKeys) -> [{Key,ListOfValues},...]`

   Takes a list of keys and returns a proplist of keys and respective list of
   values from the query string and POST values.

   **Mnemonic**: Think `qs_pl` as "Query Plurals into a Proplist"

Example:
```erlang
  wf:qs_pl([fruit,veggie,meat])

```

  Returns something like:
```erlang
  [
     {fruit,["Apple","Peach"]},
     {veggie,["Broccoli"]},
     {meat,["Pork","Beef","Venison"]}
  ].

```


* <a name="wf_request_body"></a>`wf:request_body() -> String`

   Return the complete text of the request body to the server. Note, this value
   will use the context of the current request. For example, the result of calling
   this during the page's initial request will be different than calling it within
   a postback event.

* <a name="wf_encoding"></a>`wf:encoding(Encoding) -> ok`

   Set the encoding to be applied just before sending to the client.  Valid
   values are:

  +  `none` :: No encoding. Send the response as an iolist of bytes. This is what
     you would want to use if you were returning binary data (like if you were
     generating an image file on the fly).
  +  `unicode` :: This runs the return value through Erlang's
     `unicocde:characters_to_binary/1` before sending to the client.
  +  `Function/1` :: Run the response through this particular function before
     sending, such as if you have a custom encoding you wish to use.
  +  `{Module, Function}` :: Run this through `Module:Function/1`. This is merely
     an alternative to the previous option, but one that is configuration-file
     friendly.
  +  `auto` (**Default**) :: Reads the specified response header to attempt to
     determine which encoding should be provided.  It's **quite** naive, however,
     in the name of speed.  If the header starts with "text/" or is set to
     "application/json" or "application/javascript", it will use `unicode`,
     otherwise, it will use `none`.  For example, if your page you had set
     `wf:content_type("image/png")`, it Encoding would apply as `none`.

* <a name="wf_encoding"></a>`wf:encoding() -> Encoding`

   Return the current encoding that will be applied to the response. It's worth
   noting that this will return `auto` if it's currently set to such.

* <a name="wf_status_code"></a>`wf:status_code(IntegerCode) -> ok`

   Set the HTTP response code. Default is 200.

* <a name="wf_content_type"></a>`wf:content_type(ContentType) -> ok`

   Set the HTTP content type. Defaults is `"text/html"`. This can be
   used to return text images or other files to the browser, rather than returning
   HTML.

* <a name="wf_download_as"></a>`wf:download_as(Filename) -> ok`

   Set the HTTP Content-Disposition header in such a way that the browser will
   treat the page as a download with the specified `Filename`.

* <a name="wf_request_method"></a>`wf:request_method() -> atom()`

   Return the HTTP request method used (`POST`, `GET`, `PUT`, etc), as a
   lower-case atom (example: `post`, `get`, and `put`).

* <a name="wf_url"></a>`wf:url() -> String`

   Return the complete requested URL for the page. For exaple, if the request
   is for `http://myapp.com/some/file?some_qs`, the return is
   ="http://myapp.com/some/file?some_qs".

* <a name="wf_uri"></a>`wf:uri() -> String`

   Return the complete requested URI for the page. Note, this does not include
   the protocol (http/https) or the hostname. For example, if the request is for
   `http://myapp.com/some/file?some_qs`, the return is `"/some/file?some_qs"`).

* <a name="wf_path"></a>`wf:path() -> String`

   Return the string of the requested path.  This excludes any query_string
   information. For example, if the request is for
   `http://myapp.com/some/file?some_qs` the return value is `"/some/file"`

* <a name="wf_path_info"></a>`wf:path_info() -> String`

   Return only the path info for the requested page module. In other words, if
   the module web_my_page is requsted with the path
   `"/web/my/page/some/extra/stuff` then `wf:path_info()` would return
   `"some/extra/stuff"`.

* <a name="wf_page_module"></a>`wf:page_module() -> Atom`

   Return the requested page module. Useful information to know when writing a
   custom element or action.

* <a name="wf_page_module"></a>`wf:page_module(Module) -> ok`

   Changes the current page module for the rendering engine. This will likely
   only need to be set when you need to set an otherwise valid page that
   happens to be to, for some reason, need to return a specific error. For
   example, maybe you have a `view_profile` module that expects a `userid` on
   the query-string, but the provided `userid` is invalid, so you want the page
   to throw a 404 instead.

* <a name="wf_protocol"></a>`wf:protocol() -> http | https`

   Return whether the request to the server is made by HTTP or HTTPS. Note,
   that if the request is made behind a proxy, this will only return whether
   the request from the proxy to the server is in HTTP or HTTPS, so the
   protocol seen by Nitrogen might not be the same protocol used by the client.

* <a name="wf_peer_ip"></a>`wf:peer_ip() -> IPAddress (4-tuple or 8-tuple)`

   Return the IP address of the client. Note that Erlang IP Addresses are
   4-tuples (IPv4) or 8-tuples (IPv6). For example: `{127,0,0,}` or
   `{8193,3512,51966,47806,0,0,0,1}`

* <a name="wf_peer_ip"></a>`wf:peer_ip(ListOfProxies) -> IPAddress (4-tuple or 8-tuple)`

   Shortcut method for `wf:peer_ip(ListOfProxies, x_forwarded_for)`

* <a name="wf_peer_ip"></a>`wf:peer_ip(ListOfProxies, ForwardedHeader) -> IPAddress (4-tuple or 8-tuple)`

   This will compare the `peer_ip` address against the provided list of proxies,
   and if any of them match the connected IP, then return the IP address from
   the `ForwardedHeader`.

## Cookies

* <a name="wf_cookies"></a>`wf:cookies() -> [{AtomKey, StringValue}, ...].`

   Return a proplist of all cookies.

* <a name="wf_cookie"></a>`wf:cookie(Key) -> String`

   Get the value of a cookie.

* <a name="wf_cookie_default"></a>`wf:cookie_default(Key, Default) -> String.`

   Get the value of a cookie, if it doesn't exist, return the default.

* <a name="wf_cookie"></a>`wf:cookie(Key, Value) -> ok`

   Tell Nitrogen to set a cookie in the browser. Uses `"/"` for the Path, and
   Nitrogen's session timeout setting for the MinutesToLive value.

* <a name="wf_cookie"></a>`wf:cookie(Key, Value, Options) -> ok`

   Tell Nitrogen to set a cookie on the browser with the provided `Options`.  `Options`
   is a proplist that looks something like this:

   ```erlang
   Options = [
     {domain, "example.com"},
     {path, "/"},
     {max_age, 3600}, %% expire in an hour (3600 seconds)
     {secure, true},   %% secure only?
     {http_only, true},  %% HTTP-only means cookie can't be accessed with Javascript
     {same_site, lax}  %% CSRF protection option. `lax` is usually recommended
   ]
   ```
   The values of `Options` can be reviewed on [Mozilla's documentation for the Set-Cookie Header](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie)
 
* <a name="wf_cookie"></a>`wf:cookie(Key, Value, Path, MinutesToLive) -> ok`

   Tell Nitrogen to set a cookie in the browser under the specified Path that is valid
   for a certain number of minutes.

* <a name="wf_delete_cookie"></a>`wf:delete_cookie(Key) -> ok`

   Tell Nitrogen to set the cookie to expire immediately, effectively deleting
   it from the browser.  Is a shortcut for `wf:cookie(Key,"","/",0)`.

* <a name="wf_script_nonce"></a>`wf:script_nonce() -> String`

    Get the value of the nonce in the current context.  See [Content
    Security Policy](csp.md) for more information.

## HTTP Headers

* <a name="wf_headers"></a>`wf:headers() -> [{AtomKey, StringValue}, ...]`

   Return a proplist of all HTTP headers.

* <a name="wf_header"></a>`wf:header(AtomKey) -> Value`

   Get the value of an HTTP header.

* <a name="wf_header_default"></a>`wf:header_default(AtomKey, Default) -> Value.`

   Get the value of an HTTP header, if it doesn't exist, return the default.

* <a name="wf_header"></a>`wf:header(StringKey, HeaderValue) -> ok`

   Set an HTTP header during the next response.

## Serialization

* <a name="wf_pickle"></a>`wf:pickle(Term) -> PickledBinary`

   Serialize a term into a web-safe, checksummed, and AES-encrypted hex string.

* <a name="wf_depickle"></a>`wf:depickle(PickledBinary) -> Term`

   Turn a pickled binary back into the original term.

* <a name="wf_depickle"></a>`wf:depickle(PickledBinary, SecondsToLive) -> Term or 'undefined'`

   Turn a pickled binary back into the original term, checking to see
   if the term was pickled more than `SecondsToLive` second
   ago. Returns the `Term` if it is still 'fresh' or the atom
   `'undefined'`.

## Logging

* <a name="wf_info"></a>`wf:info(String)`

   Log an informational message.

* <a name="wf_info"></a>`wf:info(Format, Args)`

   Log an informational message.

* <a name="wf_warning"></a>`wf:warning(String)`

   Log a warning message.

* <a name="wf_warning"></a>`wf:warning(Format, Args)`

   Log a warning message.

* <a name="wf_error"></a>`wf:error(String)`

   Log an error message.

* <a name="wf_error"></a>`wf:error(Format, Args)`

   Log and error message.

* <a name="wf_console_log"></a>`wf:console_log(Terms)`

   Send a `console.log()` to the browser with `Terms` as the value. (See
   [Console Log Action](console_log.md))

## Configuration

* <a name="wf_config"></a>`wf:config(Key) -> Term`

   Get the Nitrogen configuration setting under the specified Key.

* <a name="wf_config_default"></a>`wf:config_default(Key, DefaultValue) -> Term`

   Get the Nitrogen configuration setting under the specified Key. If
   not set, then return DefaultValue.
