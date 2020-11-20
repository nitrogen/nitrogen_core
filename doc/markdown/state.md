<!-- dash: State Handler | Guide | ###:Section -->



## State Handler

  The State handler controls how the `wf:state` functions store and retrieve
  page-state variables.  State variables are defined as being page-load
  specific. That is, a loading or reloading a page should result in an empty
  page state. Page state, however, is retained with postbacks, comet processes,
  and other asynchronous requests.

  Bear in mind that the state is not a **secure** store. That is, page-state gets
  pickled and sent to the client, so anything that needs to be hidden from the
  client is not recommended to be stored in the state.
 
  The default implementation of the page-state is as a simple Erlang proplist.

### Behavior Functions
 
##### `init(Config, State)`

  Initialize the state handler

 *  /Return Value/ - `{ok, NewState}` 

##### `finish(Config, State)`

  Clean up the handler

 *  /Return Value/ - `{ok, NewState}`
  
##### `get_state(Key, DefaultValue, Config, State)`
  
  Retrieves the current value of the state variable with the identifier `Key`

 *  `Key` - The identifier to search for

 *  `DefaultValue` - If `Key` is not found, return this value instead.

 *  /Return Value/ - The found value associated with `Key` (or `DefaultValue`)

##### `set_state(Key, Value, Config, State)`

  Stores `Value` in the page state with the identifier `Key`

 *  `Key` - The identifer to store

 *  `Value` - The value to store

 *  /Return Value/ - `{ok, NewState}`

##### `clear(Key, Config, State)`

  Clears the page state value associated with `Key`

 *  `Key` - The identifier to delete from the page state

 *  /Return Value/ - `{ok, NewState}`

##### `clear_all(Config, State)`

  Wipes out all page state variables.

 *  /Return Value/ - {ok, NewState}

### Example

Here is the complete text of the default state handler

```erlang
-module (default_state_handler).
-behaviour (state_handler).
-include_lib ("wf.hrl").
-export ([
    init/2, finish/2, get_state/4, set_state/4, clear/3, clear_all/2
]).

init(_Config, State) ->
    % Deserialize the state from domState.
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

get_state(Key, DefaultValue, _Config, State) ->
    _Value = proplists:get_value(Key, State, DefaultValue).

set_state(Key, Value, _Config, State) ->
    State1 = proplists:delete(Key, State),
    State2 = [{Key, Value}|State1],
    {ok, State2}.

clear(Key, _Config, State) ->
    State1 = proplists:delete(Key, State),
    {ok, State1}.

clear_all(Config, _State) ->
    init(Config, []).

```


### See Also

 *  [Handler Overview](../handlers.md)

 *  [API: State](../api.html#sec-8)
