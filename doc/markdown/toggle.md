<!-- dash: #toggle | Event | ###:Section -->


## Toggle Action - #toggle {}

  Call JQuery UI's [toggle(effect, options, speed)](http://docs.jquery.com/UI/Effects/toggle)
  on the target element.


### Usage

```erlang
	wf:wire(myDiv, #toggle{effect=slide, speed=1000})
```

### Attributes

   * `effect` (atom) - Name of an effect. See [JQuery reference](http://docs.jquery.com/UI/Effects)
   * `options` (key/value pair) - Key/value pair of options for the specified
     effect.
   * `speed` (integer) - Speed of the effect, in milliseconds.

### See Also

 *  [base element](./action_base.md)
 *  [animate element](./animate.md)
 *  [effect element](./effect.md)

 
