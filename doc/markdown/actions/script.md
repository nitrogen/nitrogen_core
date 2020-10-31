

## Script Action - #script {}

  Run some plain Javascript.

  You can access Nitrogen elements within the Javascript using the
  obj('id') function.

  For example, if you have a Nitrogen element with the id of
  'myTextBox', then "var tb = obj('myTextBox');" will make tb point to
  the myTextBox element.

  In addition, you can use obj('me') to point to the target of the
  current action, and you can use 'parent' to get the parent of an
  element, for example: "obj('me.parent.parent')".


### Usage

```erlang
   wf:wire(myDiv, #event { type=click, actions=#script { script="gotClick(obj('me'));" } })

```

### Attributes

   * `script` (string) - The Javascript to execute.

### See Also

 *  [base element](./base.html)

 
