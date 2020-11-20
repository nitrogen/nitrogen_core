<!-- dash: #sortblock | Element | ###:Section -->



## Sort Block Element - #sortblock{}

  The sort block element allows you to make a list of Nitrogen elements sortable by the user.

### Usage

```erlang
   #sortblock { tag=block1, items=[
     #sortitem { tag=item1, body="Item 1" },
     #sortitem { tag=item2, body="Item 2" },
     #sortitem { tag=item3, body="Item 3" }
   ]}

```

### Attributes

* `tag` (Erlang term) - The term to pass into the `sort_event/2` event.
* `items` (list of #sortitem elements) - The #sortitem elements that will be sortable.
* `group` (atom or string) - The name of this sortable group.
* `connect_with_groups` (list of atoms or strings) - The other groups that can accept items from this group.
* `handle` (atom or string) - A CSS class that will be used as the drag handle for `#sortitem`s.
* `placeholder` (atom or string) - The class of the placeholder when an item is dragged away
* `force_placeholder_size` (boolean) - Force the placeholder to be the same size as the moved sortblock item

### Callbacks

* `sort_event(BlockTag, ListOfItemTags)`

Called when the user sorts elements. `BlockTag` is the tag associated with this sort block. `ListOfItemTags` is a list of item tags in the new order specified by the user.

### See Also

*  [sortitem element](./sortitem.md)
*  [Sorting Demo](http://nitrogenproject.com/demos/sorting1)
*  [Nested Sorting Demo](http://nitrogenproject.com/demos/sorting2)
