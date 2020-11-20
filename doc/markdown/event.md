<!-- dash: #event | Event | ###:Section -->



## Event Action - #event {}

  This action tells Nitrogen to listen for a client side
  event, and take some action when the event is triggered.

  An event action can trigger a postback to the server, a block of Javascript,
  other actions, or any combination of the three.

  If the 'postback' attribute is defined, the page will post back to the event/1 function
  with the postback value as the sole attribute. This postback value can be any Erlang term.

  If the 'actions' attribute is set (inherited from action base), then these actions will
  be run when this event is triggered. This attributed can be set to a list of actions, a single
  action, or an Erlang string, which will be run as Javascript.

  Within the Javascript, you can use obj('id') where id is the id of a Nitrogen element. You can
  also use obj('me') to refer to the target of the current action.

### Usage

```erlang
   wf:wire(myDiv1, #event { type=click, actions=#confirm { text="Are you sure?", postback=continue } })

```

```erlang
   wf:wire(myDiv2, #event { type=click, postback={clicked, myDiv2} })

```

### Attributes

   * `type` (atom) - Type of the event, can be any Javascript-able event, such as 'click', 'mouseover', 'mouseout', etc. Or, set to 'timer' to automatically call the event after a certain period of time.

   * `delay` (integer) - Use along with type of 'timer'. Milliseconds to delay before calling the event.

   * `postback` (Erlang term) - When this event fires, Nitrogen will initiate an event postback with the supplied term.

   * `delegate` (atom) - Delegate is the name of the module that will receive this event. Default to the current page.

### Callbacks

#### event(Tag)

    Called when the event is fired. Tag is specified in the 'postback' attribute.

### See Also

 *  [base element](./action_base.md)

 
