<!-- dash: #sync_panel | Element | ###:Section -->



## Sync Panel Element - #sync_panel {}

  The sync_panel element produces an HTML div which is hooked directly into a
  comet pool and immediately updates itself with the latest data when certain
  trigger messages are sent to that comet pool.

### Usage

```erlang
   #sync_panel {
     pool=some_pool,
     triggers=[{my, trigger}],
     body_fun=fun my_body/0
   }.


```

### Attributes

   * `pool` (Erlang Term) - The name of a global Nitrogen comet pool.

   * `triggers` (List of Erlang Terms) - A list containing trigger messages
     which when broadcast to the comet pool will trigger an update and redraw via
     the `body_fun` attribute.

   * `body_fun` (Function with arity 0) - A function which returns Nitrogen elements.

### Assisting Function

 *  element_sync_panel:refresh(CometPool, Trigger) :: Trigger a refresh on
     all `#sync_panel` elements connected to the specified `CometPool` and which
     also has the specified `Trigger` listed in its `triggers` attribute.

### See Also

 *  [base element](./element_base.md)

 *  [panel element](./panel.md)

 *  [Asynchronous Page Updates](link:../api.html#sec-5)

 *  [Demo of the #sync_panel element](https://nitrogenproject.com/demos/sync_panel)

 *  [Article about #sync_panel](http://sigma-star.com/blog/post/sync_panel)
