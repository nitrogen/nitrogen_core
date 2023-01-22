<!-- dash: Handlers - Cache | Guide | ###:Section -->



## Cache Handler

  The Cache handler basically serves as an adapter to whichever cache system
  you prefer to use.  The default caching system is called
  [nitro\_cache](https://github.com/nitrogen/nitro_cache), which is a fork of an
  older caching library called [simple\_cache](https://github.com/marcelog/simple_cache)

  Any calls to `wf:cache` will ultimately invote this cache handler.

### Behavior Functions
 
##### `init(Config, State)`

  Initialize the handler

  *  /Return Value/ - `{ok, NewState}` 

##### `finish(Config, State)`

  Clean up the handler

  *  /Return Value/ - `{ok, NewState}`
  
##### `get_cached(Key, Function, TTL, Config, State)`
  
  Check the cache and if `Key` exists, return its cached value.  If `Key` does
  not exist, execute `Fun`, store its return value in the cache and return that
  value.

  *  `Key` - The unique identifier for this cached value

  *  `Function` - A function of arity-0 that will be executed if `Key` is not found

  *  `TTL` - Time To Live: How long (in millisecons) should the return value be stored.

  *  /Return Value/ - `{ok, Value, State}` - `Value` is the return value to be sent to the caller.  `State` will be stored in the request context.

##### `set_cached(Key, Value, TTL, Config, State)`
  
  Store `Value` in the cache with `Key`
  
  *  `Key` - The unique identifier for this cached value

  *  `Value` - Any valid erlang term

  *  `TTL` - Time To Live: How long (in millisecons) should the return value be stored.

  *  /Return Value/ - `{ok, State}`

##### `clear(Key, Config, State)`
  
  Remove the `Key` from the cache, if it exists
  
  *  `Key` - The unique identifier to delete

  *  /Return Value/ - `{ok, State}`

##### `clear_all(Config, State)`
 
  Completely wipe the cache, deleting all keys and values 
  
  *  /Return Value/ - `{ok, State}`

### Example

The complete code for the default cache handler can be found [here on GitHub](https://github.com/nitrogen/nitrogen_core/blob/master/src/handlers/cache/default_cache_handler.erl)


### See Also

 *  [Handler Overview](./handlers.md)

 *  [API: Cache](./api)
