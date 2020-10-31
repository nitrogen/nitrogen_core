

## Slide Down Action - #slide_down {}

  Call JQuery's [slideDown(speed)](http://docs.jquery.com/Effects/slideDown) on the target element.

### Usage

```erlang
   wf:wire(myDiv, #slide_down { speed=500 })

```

### Attributes

   * `speed` (integer) - Speed of the effect, in milliseconds.

   * `actions` (Actions) - Nitrogen actions to perform when the element is completely hidden

### See Also

 *  [base element](./base.html)

 *  [slide_up element](./slide_up.html)

 
