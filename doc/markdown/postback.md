<!-- dash: Handlers - Postback | Guide | ###:Section -->

## Postback Handler

The postback handler.  `postback_request/2` is called on each postback
request.  The default postback handler does not implement any behavior.

### Behavior Functions

##### `init(Config, State)`

Initialize the handler.

 * /Return Value/ - `{ok, NewState}`

##### `finish(Config, State)`

Clean up the handler.

 * /Return Value/ - `{ok, NewState}`

##### `postback_request(Config, State)`

Called on each postback request.  Of particular interest are the
values of and assignment to `wf_context:event_module/0,1` and
`wf_context:event_tag/0,1`.  The example below demonstrates.

The return values is discarded; this function is called solely for its
side effects.

### Example

Here we opt to run `other_module:event/1` in place of the original
event/1 callback if the postback tag matches `{_, exception}`.

``` erlang
-module(reroute_postback_handler).
-behaviour(postback_handler).
-include_lib("wf.hrl").
-export([
          init/2
        , finish/2
        , postback_request/2
        ]).

init(_Config, State) ->
    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

postback_request(_Config, _State) ->
  case wf_context:event_tag() of
    {_, exception} -> wf_context:event_module(other_module);
    _              -> ok
  end.
```

 ### See Also

 *  [Handler Overview](./handlers.md)
