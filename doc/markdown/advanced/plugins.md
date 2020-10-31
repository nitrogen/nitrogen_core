# Nitrogen Plugins

## Plugins Overview

  Nitrogen 2.2.0 introduced a plugin system for including Nitrogen elements,
  actions, validators, modules, handlers, and static resources from external
  sources (typically as rebar dependencies).

  Generally speaking, this provides for a simpler way of handling additional
  modularity, by creating custom reusable elements that can be shared between
  applications and/or other programmers.

## Adding a plugin to your app

  The simplest way to include a plugin in your app is to include it as a rebar
  dependency. In the `deps` section of your rebar.config file, add something
  like the following:

```erlang
  {my_plugin, ".*", {git, "git://github.com/user/my_plugin.git", {branch, master}}},

```

  Then run `make` in your application. It will download the plugin, add to your
  plugins.hrl file a reference to any include files (.hrl files), copy any
  static resources into your application's directory structure, and finally
  recompile everything.

### What happens when you add a plugin to your app

   The Nitrogen Plugin installer does a handful of things to your Application
   directory structure.  Any static resources are automatically copied into a
   `site/static/plugins/<pluginname>` directory.  Any Erlang includes (.hrl
   files) are added as `-include` calls to the `site/include/plugins.hrl` file.
   And, of course, being an Erlang dependency, the source code for the plugin
   will be automatically loaded.

   The directories above can all be customized with the plugins.config file
   found in the root of your Nitrogen release (see the "plugins.config" section
   of [Configuration Options](config.html) for how to customize your
   plugin configuration.

## Finding plugins to include in your app

  Currently, the list of available plugins are found on our
  [Plugins Wiki Page](https://github.com/nitrogen/nitrogen/wiki/Nitrogen-Plugins)
  on GitHub.

  If you've created your own plugins, feel free to include it in that list.

## Creating your own plugins

### Sample Nitrogen Plugin

  The simplest way to create your own plugin is to start with the
  [Sample Nitrogen Plugin](https://github.com/nitrogen/sample_nitrogen_plugin)
  and expand on it to make your own plugin.

### Creating your plugin from scratch

   Creating a reusable plugin from scratch is a relatively simple task, and it
   requires following the following steps:

   1) A Nitrogen plugin must be a valid Erlang application, that is, it has
      either a .app file in the ebin directory, or a .app.src file in the src
      directory.

   2) In order for Nitrogen to determine that an application is indeed a
      Nitrogen plugin, the root of your plugin must contain the file
      `nitrogen.plugin`. This file can be blank.

   3) Place any static resources (javascript, CSS, images, etc) in the
      `priv/static` directory of your plugin.

   4) Place any templates in the `priv/templates` directory of your plugin.

   4) Place any necessary Erlang header files (e.g. if this plugin contains
      custom elements or actions) in a directory in the root of the plugin
      and call it `include`.

   5) Finally, place your plugin's Erlang source code in the `src` directory.

## Adding plugin support to an older version of Nitrogen

  What if you have a version of Nitrogen pre-2.2.0, but you still want to add
  support to your Nitrogen app?

  There's actually no **technical** reason you can't do that.  In fact, the core
  of the plugin system is a single script, executed sometime after dependencies
  are fetched and before compilation.

  That script is called
  [do-plugins.escript](https://raw.github.com/nitrogen/nitrogen/master/rel/overlay/common/do-plugins.escript)
  and it can be found in the main Nitrogen repo. The easiest way, then to add
  the plugin system to your app is to do the following in the root of your app:

```bash
  wget https://raw.github.com/nitrogen/nitrogen/master/rel/overlay/common/do-plugins.escript
  chmod 755 do-plugins.escript

```

  Then simply modify your Makefile so that `./do-plugins.escript` is executed
  before `./rebar compile`

  For a good practical example of this, check out the standard
  [Makefile](https://github.com/nitrogen/nitrogen/blob/master/rel/overlay/common/Makefile#L20)
  that comes with a Nitrogen install.

## See Also

 *  [Configuration Options](config.html)

 *  [Sample Nitrogen Plugin](https://github.com/nitrogen/sample_nitrogen_plugin)
