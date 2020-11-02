

## Identity Handler

The identity handler ties directly with the `wf:user` functions, the default
approach is to simply store the user information into a specific session
variable.

### Behavior Functions
 
##### `init(Config, State)`

  Initialize the handler

 *  /Return Value/ - `{ok, NewState}` 

##### `finish(Config, State)`

  Clean up the handler

 *  /Return Value/ - `{ok, NewState}`
  
##### `get_user(Config, State)`

  Retrieve the previously stored user information.

 *  /Return Value/ - The previously stored user information.

##### `set_user(User, Config, State)`

  Set the user information

 *  `User` - Free-form user information. Can be any erlang term: a username,
             user ID, tuple, proplist, Erlang record, etc..

 *  /Return Value/ - {ok, NewState}

##### `clear(Config, State)`
  
  Clear the user information.

 *  /Return Value/ - {ok, NewState}

### Example

Here is the complete text of the default identity handler, which uses simply
stores the data in sessions.

```erlang

-module (default_identity_handler).
-behaviour (identity_handler).
-export ([
    init/2,
    finish/2,
    get_user/2,
    set_user/3,
    clear/2
]).
-define(KEY, {default_identity_handler, user}).

init(_Config, State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

get_user(_Config, _State) ->
    wf:session(?KEY).

set_user(User, _Config, State) ->
    wf:session(?KEY, User),
    {ok, State}.

clear(_Config, State) ->
    wf:session(?KEY, undefined),
    {ok, State}.
~


```


### See Also

 *  [Handler Overview](../handlers.html)

 *  [API: Authentication and Authorization](../api.html#sec-9)
