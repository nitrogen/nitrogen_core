

## Hide Action - #hide{}

  If an effect is specified, then call JQuery UI's [hide(effect, options, speed)](http://docs.jquery.com/UI/Effects/hide).
	
  Otherwise, just hide the target element.

### Usage

```erlang
   wf:wire(myDiv, #hide { effect=slide, speed=500 })

```

### Attributes

   * `effect` (atom) - Name of an effect. See <a href='http://docs.jquery.com/UI/Effects'>JQuery reference</a>.

   * `options` (key/value pair) - Key/value pair of options for the specified effect.

   * `speed` (integer) - Speed of the effect, in milliseconds.

### See Also

 *  [base element](./action_base.md)

 *  [show element](./show.html)

 
