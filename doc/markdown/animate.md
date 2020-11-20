<!-- dash: #animate | Event | ###:Section -->



## Animate Action - #animate {}

  Call JQuery UI's [animate(options, speed, easing)](http://docs.jquery.com/Effects/animate) on the target element.

### Usage

```erlang
   wf:wire(myDiv, #animate { options=[{width, 200}, {fontSize, "3em"}], speed=200 })

```

### Attributes

   * `options` (key/value pairs) - CSS options to set during the animation.

   * `speed` (integer) - Speed of the effect, in milliseconds.

   * `easing` (atom) - Name of a JQuery easing.

### See Also

 *  [base element](./action_base.md)

 *  [effect element](./effect.md)

 *  [toggle element](./toggle.md)

 
