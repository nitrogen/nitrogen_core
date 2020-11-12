

## Config Handler

  The config handler controls how the configuration settings are loaded. This
  is probably not something that's likely to get changed.

### Behavior Functions
 
##### `init(Config, State)`

  Initialize the handler

 *  /Return Value/ - `{ok, NewState}` 

##### `finish(Config, State)`

  Clean up the handler

 *  /Return Value/ - `{ok, NewState}`
  
##### `get_value(Key, DefaultValue, Config, State)`

  Search the configuration for the key and return the value

 *  `Key` - The configuration key to find

 *  `DefaultValue` - If the `Key` is not found, return the provided value

 *  /Return Value/ - The value of the found key

##### `get_values(Key, DefaultValue, Config, State)`

  Search the configuration and return a list of found values,
  if the configuration system you're using can return multiple
  values for a single key.

 *  `Key` - The configuration key to find

 *  `DefaultValue` - If the `Key` is not found, return the provided value

 *  /Return Value/ - The values of the found key

### Example

```erlang
-module (default_config_handler).
-include_lib ("wf.hrl").
-behaviour (config_handler).
-export ([
    init/2,
    finish/2,
    get_value/4,
    get_values/4
]).

init(_Config, _State) ->
    {ok, []}.

finish(_Config, _State) ->
    {ok, []}.

get_value(Key, DefaultValue, Config, State) ->
    case get_values(Key, [DefaultValue], Config, State) of
        [Value] ->
            Value;
        Values ->
            error_logger:error_msg("Too many matching config values for key: ~p~n", [Key]),
            throw({nitrogen_error, too_many_matching_values, Key, Values})
    end.

get_values(Key, DefaultValue, _Config, _State) ->
    case application:get_env(nitrogen, Key) of
        {ok, Value} ->
            [Value];
        undefined ->
            DefaultValue
    end.

```

### See Also

 *  [Handler Overview](../handlers.md)

 *  [API: Configuration](../api.html#sec-15)
