<!-- dash: #droppable | Element | ###:Section -->



## Droppable Element

  The droppable element allows you to make a block of Nitrogen elements a drop
  target for draggable elements.

  Combine the draggable element with the droppable element to allow drag and
  drop behavior.

### Usage

```erlang
   #droppable { tag=drop1, accept_class=accept, hover_class=hover, body=[
     #span { text="This is a droppable block." }
   ]}

```

### Attributes

   * `tag` (Erlang term) - The drag term to pass into the drop_event/2
     event.

   * `body` (Nitrogen elements) - The elements that will be droppable.

   * `accept_groups` (list of atoms (defaults to 'all')) - The drag groups
     that will be accepted by this drop target

   * `active_class` (atom or string (defaults to 'active')) - This CSS
     class will be applied to the element when the user drags a draggable
     element.

   * `hover_class` (atom or string (defaults to 'hover')) - This CSS class
     will be applied to the element when the user hovers with a draggable
     element.

### Events

#### drop_event(DragTag, DropTag)

   Called when the user drops an element. DragTag is the tag associated with
   the draggable element. DropTag is the tag associated with the droppable
   element.

### See Also

 *  [draggable element](./draggable.md)
