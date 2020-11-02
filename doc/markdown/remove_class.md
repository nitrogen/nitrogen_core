

## Remove Class Action - #remove_class {}

  Call JQuery UI's [removeClass(class, speed)](http://docs.jquery.com/UI/Effects/removeClass) on the target element.

### Usage

```erlang
   wf:wire(myDiv, #remove_class { class=selected, speed=1000 })

```

### Attributes

   * `class` (atom) - Name of the class.

   * `speed` (integer) - Speed of the effect, in milliseconds.

### See Also

 *  [base element](./action_base.md)

 *  [Add Class](add_class.md)

 
