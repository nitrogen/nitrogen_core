

## Show Action - #show {}

  If an effect is specified, then call JQuery UI's [show(effect,
  options, speed)](http://docs.jquery.com/UI/Effects/show) on the target element.
	
  Otherwise, just show the element.


### Usage

```erlang
   wf:wire(myDiv, #show { effect=slide, speed=500 })

```

### Attributes

   * `effect` (atom) - Name of an effect. See <a href='http://docs.jquery.com/UI/Effects'>JQuery reference</a>.

   * `options` (key/value pair) - Key/value pair of options for the specified effect.

   * `speed` (integer) - Speed of the effect, in milliseconds.

### See Also

 *  [base element](./action_base.md)

 *  [hide element](./hide.html)

 
