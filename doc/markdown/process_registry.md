

## Process Registry Handler

The process registry handler is a simple key-value store specifically for
  storing and retrieving Erlang Pids from Keys. It's primary uses are in
  session tracking and in tracking Pids for [comet](../doc/api.html#sec-5).
  The default nitrogen process registry is based around
  [nprocreg](https://github.com/nitrogen/nprocreg), which connects and
  synchronizes Nitrogen nodes' process registries together.

### Behavior Functions
 
##### `init(Config, State)`

  Initialize the Process Registry handler

 *  /Return Value/ - `{ok, NewState}` 

##### `finish(Config, State)`

  Clean up the Process Registry handler

 *  /Return Value/ - `{ok, NewState}`
  
##### `get_pid(Key, Config, State)`
  
  Returns a Pid from the provided `Key`.

 *  `Key` - Any erlang term to use as a key. For the session handler, Nitrogen
      uses the SessionID, while for the comet functions, Nitrogen uses the
      comet pool ID, or a timer Pid. The point is, `Key` can be just about
      anything.

 *  /Return Value/ - `{ok, Pid, NewState}`.  If `Key` is not found, then `Pid` will be `undefined`. 

##### `get_pid(Key, Function, Config, State)`
  
  Returns a Pid from the provided `Key`.

 *  `Key` - Same as above.

 *  `Function` - If `Key` is not found, the process registry should spawn off a
      new process using the provided `Function` (which should have arity of 0), then return the pid
      of the newly spawned process.

 *  /Return Value/ - `{ok, Pid, NewState}`. If `Key` is not found, then `Pid`
      will be the Pid of the newly spawned process based on `Function`.

### Example

Here is the complete text of the default process registry handler
(nprocreg_process_registry), which is very simple.

```erlang
-module (nprocreg_registry_handler).
-behaviour (process_registry_handler).
-include_lib ("wf.hrl").
-export ([
    init/2,
    finish/2,
    get_pid/3,
    get_pid/4
]).

init(_Config, State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

get_pid(Key, _Config, State) ->
    Pid = nprocreg:get_pid(Key),
    {ok, Pid, State}.

get_pid(Key, Function, _Config, State) ->
    Pid = nprocreg:get_pid(Key, Function),
    {ok, Pid, State}.

```


### See Also

 *  [Handler Overview](../handlers.md)
