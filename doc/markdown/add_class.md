
## Add Class Action - #add_class {}

  Call JQuery UI's [addClass(class, speed)](http://docs.jquery.com/UI/Effects/addClass) on the target element.

### Usage

```erlang
   wf:wire(myDiv, #add_class { class=selected, speed=1000 })

```

### Attributes

   * `class` (atom) - Name of the class.

   * `speed` (integer) - Speed of the effect, in milliseconds.

### See Also

 *  [base element](./action_base.md)

 *  [Remove Class](remove_class.md)

 
