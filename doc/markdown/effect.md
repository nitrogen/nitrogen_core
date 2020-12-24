<!-- dash: #effect | Event | ###:Section -->



## Effect Action - #effect {}

  Call JQuery UI's [effect(effect, options, speed)](http://docs.jquery.com/UI/Effects/effect) on the target element.

### Usage

```erlang
   wf:wire(myDiv, #effect { effect=highlight, speed=500 })

```

### Attributes

   * `effect` (atom) - Name of an effect. See <a href='http://docs.jquery.com/UI/Effects'>JQuery reference</a>.

   * `options` (key/value pair) - Key/value pair of options for the specified effect.

   * `speed` (integer) - Speed of the effect, in milliseconds.

### See Also

 *  [base element](./action_base.md)

 *  [animate element](./animate.md)

 *  [toggle element](./toggle.md)

 
