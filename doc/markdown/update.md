

## Update Action - #update{}

  The `#update{}` action serves as a wrapper for a handful of ways to modify
  the contents of a page without having to do it using `wf:update`,
  `wf:insert_after`, etc.

### All actions that work with this

 *  `#update{}`
 *  `#replace{}`
 *  `#insert_top{}`
 *  `#insert_bottom{}`
 *  `#insert_before{}`
 *  `#insert_after{}`
 *  `#remove{}`

###  Usage

```erlang
  wf:wire(my_button, #event{type=click, actions=[
    #update{target=someDiv, elements="Replace All"}
  ]}).

```

```erlang
  wf:wire(my_button, #event{type=click, actions=[
    #remove{target=someOtherDiv}
  ]}).

```

```erlang
  wf:wire(my_button, #event{type=click, actions=[
    #insert_after{target=someOtherDiv, elements=[#button{text="A new button"}]
  ]}).

```

### Attributes

   * `elements` (Nitrogen Elements) - Specify a body for the action to insert, update, or replace. This attribute is ignored by `#remove{}`.

   * `type` (atom) - Which update mechanism to use. Generally, it's
   recommended to ignore this attribute unless you have a reason to dynamically
   assign which mechanism to use.  By default, this is set to the same value as
   the action record used (so `#update{}`'s `type` attribute is set to
   `update`, while `#insert_before{}`'s `type` attribute is set to
   `insert_before`.

### See Also

 *  [base element](./action_base.md)
