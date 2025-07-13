<!-- dash: Handlers | Guide | ##:Section -->

# Advanced Routing Hooks Configuration

The Nitrogen Web Framework, through its simple_bridge library, provides powerful hooks for direct 
route manipulation with both Cowboy and Webmachine web servers. 
These configuration options allow developers to bypass the default routing mechanisms and implement 
custom dispatch logic for advanced use cases.

## Overview

The routing hooks are configured in the `simple_bridge.config` file and provide several strategies for customizing how HTTP requests are dispatched to your application handlers. These hooks are particularly useful when you need to:

- Implement custom API endpoints outside the standard Nitrogen page system
- Integrate with existing routing logic
- Create specialized handlers for specific URL patterns
- Implement RESTful services alongside your Nitrogen application

## Configuration Options

### Webmachine Dispatch Configuration

#### `webmachine_dispatch`
```erlang
{webmachine_dispatch, DispatchTable}
```

This option allows you to override the default dispatch table with a custom Webmachine dispatch configuration. 

**Parameters:**
- `DispatchTable`: A valid Webmachine dispatch table structure

**Example:**
```erlang
{webmachine_dispatch, [
    {["api", "users", id], my_user_resource, []},
    {["api", "posts"], my_posts_resource, []}
]}
```

#### `webmachine_dispatch_fun`
```erlang
{webmachine_dispatch_fun, {Module, Function}}
```

This option specifies a Module:Function() combination that will be evaluated at runtime to generate a dispatch table for Webmachine. This provides dynamic dispatch table generation and also ignores the `static_paths` configuration.

**Parameters:**
- `Module`: The Erlang module containing the dispatch function
- `Function`: The function name that returns a dispatch table

**Example:**
```erlang
{webmachine_dispatch_fun, {my_app_routes, get_webmachine_dispatch}}
```

The corresponding function might look like:
```erlang
-module(my_app_routes).
-export([get_webmachine_dispatch/0]).

get_webmachine_dispatch() ->
    [
        {["api", "v1", '*'], api_handler, []},
        {["admin", '*'], admin_handler, []}
    ].
```

### Cowboy Dispatch Configuration

#### `cowboy_dispatch`
```erlang
{cowboy_dispatch, DispatchTable}
```

Similar to the Webmachine option, this allows you to override the default dispatch table with a custom Cowboy dispatch configuration.

**Parameters:**
- `DispatchTable`: A valid Cowboy dispatch table structure

**Example:**
```erlang
{cowboy_dispatch, [
    {'_', [
        {"/api/users/[...]", cowboy_rest, [{handler, user_handler}]},
        {"/api/posts/[...]", cowboy_rest, [{handler, post_handler}]}
    ]}
]}
```

Or then if `{dispatch_strategy, merge}`:

```erlang
{cowboy_dispatch, [    
    {"/api/users/[...]", cowboy_rest, [{handler, user_handler}]},
    {"/api/posts/[...]", cowboy_rest, [{handler, post_handler}]}    
]}
```



#### `cowboy_dispatch_fun`
```erlang
{cowboy_dispatch_fun, {Module, Function}}
```

Specifies a Module:Function() that will be evaluated to return a dispatch table for Cowboy. This function-based approach allows for dynamic route generation and ignores the `static_paths` configuration.

**Parameters:**
- `Module`: The Erlang module containing the dispatch function
- `Function`: The function name that returns a dispatch table

**Example:**
```erlang
{cowboy_dispatch_fun, {api_routes, cowboy_routes}}
```

The corresponding function implementation:
```erlang
-module(api_routes).
-export([cowboy_routes/0]).

cowboy_routes() ->
    [
        {'_', [
            {"/api/auth/login", auth_handler, [{action, login}]},
            {"/api/auth/logout", auth_handler, [{action, logout}]},
            {"/api/data/[...]", data_handler, []},
            {"/ws/[...]", websocket_handler, []}
        ]}
    ].
```

Or then if `{dispatch_strategy, merge}`:
```erlang
-module(api_routes).
-export([cowboy_routes/0]).

cowboy_routes() ->
    [
        {"/api/auth/login", auth_handler, [{action, login}]},
        {"/api/auth/logout", auth_handler, [{action, logout}]},
        {"/api/data/[...]", data_handler, []},
        {"/ws/[...]", websocket_handler, []}
    ].
```

#### `dispatch_strategy`
```erlang
{dispatch_strategy, Strategy}
```

This configuration determines how custom Cowboy dispatch tables are merged with the default Nitrogen routing.

**Parameters:**
- `Strategy`: Either `merge` or `override`
  - `merge`: Combines your custom routes with Nitrogen's default routes
  - `override`: Completely replaces Nitrogen's default routing with your custom routes. 

**Example:**
```erlang
{dispatch_strategy, merge}
```

## Best Practices

### 1. Strategy Selection
- Use `merge` strategy when you want to add custom API endpoints alongside standard Nitrogen pages
- Use `override` strategy when you need complete control over all routing. See `cowboy_simple_bridge_sup.erl` and `webmachine_simple_bridge_sup.erl` to override with new implementation logic.

## Integration with Nitrogen

When using these advanced routing hooks, remember that:

1. **Static Paths Ignored**: If `{dispatch_strategy, override}`, all `*_dispatch` and `*_dispatch_fun` options ignore the `static_paths` configuration
2. **Handler Responsibility**: Custom handlers are responsible for serving static files if needed
3. **Nitrogen Compatibility**: Ensure your custom routes don't conflict with Nitrogen's internal routing
4. **Session Management**: Custom handlers may need to integrate with Nitrogen's session management

## Troubleshooting

### Common Issues

1. **Routes Not Working**: Verify that your dispatch function returns a valid dispatch table structure. 
When you use cowboy a dn `{dispatch_strategy, override}`, you must provide a complete dispatch table in cowboy format. When you use `{dispatch_strategy, merge}`, provide a list of routes.
2. **Static Files Not Served**: Remember that custom dispatch `{dispatch_strategy, override}` ignores `static_paths`
3. **Conflicts with Nitrogen**: Ensure custom routes don't override essential Nitrogen endpoints

## Conclusion

The advanced routing hooks in simple_bridge provide powerful capabilities for integrating custom routing logic with the Nitrogen Web Framework. Whether you need to add REST API endpoints, implement custom handlers, or create dynamic routing based on runtime conditions, these configuration options give you the flexibility to extend Nitrogen's capabilities while maintaining its core benefits.
