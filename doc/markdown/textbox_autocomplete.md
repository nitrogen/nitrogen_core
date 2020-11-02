
## Textbox Autocomplete Element - #textbox_autocomplete {}

  The textbox_autocomplete element produces an HTML textbox with built-in
  autocomplete functionality through the use of server postbacks for selecting
  and element and typing an element

### Usage

```erlang
   #textbox_autocomplete { tag=lookup }

```

### Events

```erlang
   autocomplete_enter_event(SearchTerm, Tag) ->
      _JsonString = some_search_function(SearchTerm),
      
   autocomplete_select_event({struct, [{<<"id">>,ID }, {<<"value">>, Value}]}, Tag) ->
      wf:flash(wf:f("Selected ~p:~p",[ID, Value])).  

```

### Core Attributes

   Core attributes copied from [#textbox{}](textbox.md)

### Attributes

 *  tag (*Erlang Term*)  :: The tag to be passed to the
      `autocomplete_enter_event` and `autocomplete_select_event` functions.

### See Also

 *  [base element](./base.html)

 *  [button element](./button.html)

 *  [textbox element](./textbox.html)

 *  [textarea element](./textarea.html)

 *  [checkbox element](./checkbox.html)

 *  [dropdown element](./dropdown.html)

 *  [option element](./option.html)

 *  [Textbox Autocomplete Demo](http://nitrogenproject.com/demos/textbox_autocomplete)
 
