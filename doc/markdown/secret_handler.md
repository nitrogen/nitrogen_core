<!-- dash: Template Handler | Guide | ###:Section -->

## Secret Handler

The secrets handler controls how Nitrogen can read secrets (such as API keys or
service passwords). The default secret handler simply reads an Erlang config

### Behavior Functions

##### `init(Config, State)`

Initialize the handler

- _Return Value_ - `{ok, NewState}`

##### `finish(Config, State)`

Clean up the handler

- _Return Value_ - `{ok, NewState}`

##### `get_value(Key, Default, Config, State)`

Retrieve the value associated with the provided `Key`.

- `Key` - The key to query from the secrets manager.

- `Default` - This is the value to return if the key is not found.

- _Return Value_ - `Any Erlang Term`

### Example

Here is the complete text of the default secret handler. You'll notice the
default one is quite basic, and relies on the `nitro_secret` application.

```erlang
-module(default_secret_handler).
-include("wf.hrl").
-behaviour(secret_handler).
-export ([
    init/2,
    finish/2,
    get_value/4
]).

init(_Config, State) ->
    {ok, State}.

finish(_Config, _State) ->
    {ok, []}.

get_value(Key, Default, _Config, _State) ->
    nitro_secret:get(Key, Default).
```

### See Also

- [API: wf:secret](api.html#wf_secret)

- [Secrets Management](secrets.md)

- [Handler Overview](handlers.md)
