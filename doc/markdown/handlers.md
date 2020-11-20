<!-- dash: Nitrogen Handlers | Guide | ##:Section -->


# Nitrogen Handlers

## Handler Overview

Core Nitrogen behavior has been broken out into well-defined, pluggable 
behavior modules called /handlers/. Handlers allow you to easily substitute
your own logic for things like session, security, routing, and others. Simply
create a module that implements one of the existing behaviors, and register it
to call `nitrogen:handler/2` between the `nitrogen:init_request/2` and
`nitrogen:run/0` calls found inside the `nitrogen_xxx` (where `xxx` is the
webserver used, for example 'mochiweb', 'yaws', 'cowboy', etc).

For example, let's add custom session and config handlers to our mochiweb
install. To do so, let's add our handlers to  `nitrogen_mochiweb:loop/1`:

```erlang
  loop(Req) ->
    {ok, DocRoot} = application:get_env(mochiweb, document_root),
    RequestBridge = simple_bridge:make_request(mochiweb_request_bridge, Req),
    ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, {Req, DocRoot}),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    nitrogen:handler(my_config_handler,[]),   %% Add custom config handler
    nitrogen:handler(my_session_handler,[]),  %% Add custom session handler
    nitrogen:run().

```

It may seem odd to you that we're not specifying **which** handler we're actually
loading, but that's because the `nitrogen:handler/2` function determines,
based on the `behavior()` defined within the handler module, which handler it
is we're actually loading.

## nitrogen:handler(HandlerName, Config)

`nitrogen:handler/2` is the way to function to connect your custom handler into
Nitrogen's handler system. This function takes two arguments:

 *  `HandlerName` - The name of the module.

 *  `Config` - Any configuration settings for this handler. This variable
      could be just about anything, a proplist containing API keys or IP
      addresses of related servers (say for a session handler connected to
      an external memcache or something.

## Common Handler Behavior Arguments

In all of the handler behavior functions are found two particular variables:

 *  `Config` - When a handler is initialized using `nitrogen:handler/2`, the
      second argument is `Config`, which is then passed into each handler
      function.

 *  `State` - The current state of the handler. Each handler maintains its own
      state.

## The Handlers

  **Note**: The `init/2` function is called on each handler in a specific order
  as defined in `wf_context:init_context/2`.

  In the order they are loaded, here are all the handlers, and their
  documentation:

#### Core handlers:
 *  [Config Handler](handler_config.md) - Controls how and from where the
    configuration settings are loaded.
 *  [Log Handler](log.md) - Controls the logging system for your
    Nitrogen app.
 *  [Process Registry Handler](process_registry.md) - Controls
    how processes are tracked. The default backend for the process registry
    is [nprocreg](https://github.com/nitrogen/nprocreg).
 *  [Crash Handler](crash.md) - Controls how to handle page
    crashes, including logging and how to present the crash to the user.
 *  [Query Handler](query.md) - Controls the how `wf:q` (and its
    siblings, `wf:qs`, `wf:mq`, etc), functions retrieve their values from the
    POST, GET, or other methods.

#### Stateful Handlers
 *  [Session Handler](session.md) - Controls how session
    information and variables are set, stored, and/or distributed.
 *  [State Handler](state.md) - Controls how the `wf:state`
    functions store and retrieve their values. The default uses a simple
    proplist.
 *  [Identity Handler](identity.md) - Controls how the identity
    of the client is stored and retrieved. This is related to the `wf:user`
    functions.
 *  [Role Handler](role.md) - Controls how the roles of the
    client are stored and retrieved. This is related to the `wf:role`
    functions.

#### Handlers that possibly redirect
 *  [Route Handler](route.md) - Controls how Nitrogen routes
    requests to modules or static files.
 *  [Security Handler](security.md) - Controls whether or not a user
    has access to a resource, and if not, should determine what to do with
    the request.
