<!-- dash: Role Handler | Guide | ###:Section -->



## Role Handler

  Roles in nitrogen have a specific meaning.  A user may have multiple roles,
  which will likely enable certain functionality of the site. It's analogous to
  a user having multiple groups associated with his/her Unix account.

  In Nitrogen, roles are accessed by the `wf:role` functions.

  The default role handler (like the default user handler), uses the Nitrogen
  session variables for tracking the roles.

### Behavior Functions
 
##### `init(Config, State)`

  Initialize the role handler

 *  /Return Value/ - `{ok, NewState}` 

##### `finish(Config, State)`

  Clean up the handler

 *  /Return Value/ - `{ok, NewState}`
  
##### `get_has_role(Role, Config, State)`

  Checks to see if the current user has the specified role enabled for his/her
  session.

 *  `Role` - The role to check. Can be any erlang term.

 *  /Return Value/ - `true` or `false` depending on whether or not the user is
                     is in the specified role.

##### `set_has_role(Role, IsInRole, Config, State)`

  Sets the role status for a particular role for the current user
 
 *  `Role` - The role to set the status

 *  `IsInRole` - The boolean `true` or `false`. `true` will enable the role for
                 the user, `false` will disabled the role on the user.
    
 *  /Return Value/ - {ok, NewState}

##### `get_roles(Config, State)`

  Retrieves the list of all roles currently enabled for the user.

 *  /Return Value/ - The list of roles

##### ` clear_all(Config, State)`

  Clears all roles from the current user

 *  /Return Value/ - {ok, NewState}

### Example

Here is the complete text of the default role handler

```erlang

-module (default_role_handler).
-behaviour (role_handler).
-export ([
    init/2,
    finish/2,
    get_has_role/3,
    set_has_role/4,
    get_roles/2,
    clear_all/2
]).
-define(KEY, {default_role_handler, roles}).

init(_Config, State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

get_has_role(Role, _Config, _State) ->
    Roles = wf:session_default(?KEY, []),
    lists:member(Role, Roles).

set_has_role(Role, IsInRole, _Config, State) ->
    Roles = wf:session_default(?KEY, []),
    Roles1 = Roles -- [Role],
    case IsInRole of
        true -> wf:session(?KEY, [Role|Roles1]);
        _    -> wf:session(?KEY, Roles1)
    end,
    {ok, State}.

get_roles(_Config, _State) ->
    wf:session_default(?KEY, []).

clear_all(_Config, State) ->
    wf:session(?KEY, []),
    {ok, State}.


```


### See Also

 *  [Handler Overview](../handlers.md)

 *  [API: Authentication and Authorization](../api.html#sec-9)
