<!-- dash: #textbox_autocomplete | Event | ###:Section -->


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

 *  [base element](./element_base.md)

 *  [button element](./button.md)

 *  [textbox element](./textbox.md)

 *  [textarea element](./textarea.md)

 *  [checkbox element](./checkbox.md)

 *  [dropdown element](./dropdown.md)

 *  [option element](./option.md)

 *  [Textbox Autocomplete Demo](http://nitrogenproject.com/demos/textbox_autocomplete)
 
