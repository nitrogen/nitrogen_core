

## Slide Up Action - #slide_up {}

  Call JQuery's [slideUp(speed)](http://docs.jquery.com/Effects/slideUp) on the target element.

### Usage

```erlang
   wf:wire(myDiv, #slide_up { speed=500 })

```

### Attributes

   * `speed` (integer) - Speed of the effect, in milliseconds.

   * `actions` (Actions) - Nitrogen actions to perform when the element is completely hidden

### See Also

 *  [base element](./base.html)

 *  [slide_down element](./slide_down.html)

 
