<!-- dash: Server Specific Options | Guide | ##:Section -->


# Server-Specific Options

## Server-Specific Overview

  Prior to Nitrogen 2.3 and SimpleBridge 2.0, each server (Cowboy, Yaws,
  Mochiweb, etc), required its own custom configuration.  For those of you
  running those older version, or opting to use your own server-specific custom
  configuration, these should help you out some.

  (Keep in mind this document is not likely to be updated further, as
  SimpleBridge is the recommended method for running Nitrogen).

## Common Configurations:

  In most of the configurations below, you'll find some general running themes.
  Almost all of the servers take same basic set of core instructions to for
  initialization and address binding, as well as some way to notify Nitrogen
  and SimpleBridge where to find static files.  These universal configuration
  settings are below:

    * `bind_address` (String) - The string of the IP address to bind.  If set to "0.0.0.0" or left blank, it'll
      bind to all available addresses. (Default: `"0.0.0.0"` )

    * `port` (Number) - The port number to bind. (Default: `8000`)

      **About Ports and Linux**: While port 80 is the standard HTTP port,port 80 is a
      privileged port in a Linux/Unix environment. This means that in order for
      Erlang to bind to port 80, it will need to be run with **root** privileges. This
      is generally unadvised. Instead, we recommend using a lightweight reverse
      proxy (such as nginx) in front of Nitrogen. Doing so will allow you to run
      Nitrogen with standard user privileges (for better system security), while
      presenting your Nitrogen website on the expected port 80.

      On some variants of Linux, it is possible to bind Nitrogen to port 80 without
      running as root. This is accomplished with the use of the
      [`setcap`](http://linux.die.net/man/8/setcap) application (which may need to
      be installed from your distro's package system).

      An example of `setcap` being run on your Erlang app:

```bash
        sudo setcap cap_net_bind_service+ep ./erts-5.9.2/bin/beam
        sudo setcap cap_net_bind_service+ep ./erts-5.9.2/bin/beam.smp

```

      This will give the `beam` and `beam.smp` programs privileges to bind to
      privileged ports (ports under 1024).

    * `server_name` (Erlang term) - What to name the server. (Default: `nitrogen`)

    * `document_root` (String) - The root of the location of static resources (ie, stylesheets, javascript
      files, images, etc). This will be passed to simple_bridge for the serving of
      static files. (Default: `"./site/static"` )

      **Note:** this is relative to the root of the Nitrogen installation.

## The Servers (in alphabetical order)

### Cowboy: etc/cowboy.config
   m

  [Cowboy](http://github.com/extend/cowboy) is the web server made by
  [Loïc Hoguin](http://twitter.com/lhoguin). The configuration for Cowboy
  provided with Nitrogen isn't the /official/ configuration file, but it works
  for our purposes.  The options we provide are read by Nitrogen and passed to
  the Cowboy server upon initialization.

  The default Cowboy configuration is as follows:

```erlang
  [
      {cowboy,[
          {bind_address,"0.0.0.0"},
          {port,8000},
          {server_name,nitrogen},
          {document_root,"./site/static"},
          {static_paths, ["js/","images/","css/","nitrogen/"]}
      ]}
  ].

```

 *  `static_paths` - (*List of Strings*) 

    This setting will be used to determine if a requested resource should be
    handled by Nitrogen and simple_bridge, or if it should just be immediately
    served directly by the Cowboy server. 
    (Default: `["js/","images/","css/","nitrogen/"]`)

     **Note 1:** This is relative to the `document_root` above. So requests for `js/`
    will be served from `./site/static/js/` (using the default above).

     **Note 2:** it is **strongly** recommended to catch static files with the
    `static_paths` setting. simple_bridge does not serve large static files in an
    optimal way (it loads the files into memory completely before sending).

### Inets: etc/inets.config and etc/inets_httd.erlenv

  [Inets](http://www.erlang.org/doc/man/inets.html) is the web client and
  server included with the standard Erlang Install, and we use the Inets Web
  server as the "simple" solution for Nitrogen. Inets isn't as feature-rich as
  the other popular Erlang webservers, and because of this, we only recommend
  using Inets for development purposes, since it doesn't require any additional
  installation.

  Further, the Inets configuration is broken into two different files, one for
  the `inets` application itself, and one for the httpd server included in Inets.

##### etc/inets.config

  This is the file for configuring the `inets` application itself. By default, we
  simply use this file to tell the application to start the httpd and load the
  specified configuration file.

  The default inets.config provided with Nitrogen is as follows:

```erlang
  [{inets, [
      {services, [
          {httpd, [
              {proplist_file, "./etc/inets_httpd.erlenv"}
          ]}
      ]}
  ]}].

```

  Note that basically all it does it tell `inets` to load the inets_httpd.erlenv
  proplist file, using the `proplist_file` option.

##### etc/inets_httpd.erlenv

  This file does the heavy lifting of configuring our inets configuration.

```erlang
  [
      {port, 8000},
      {bind_address, {0,0,0,0}},
      {server_name, "nitrogen"},
      {server_root, "."},
      {document_root, "./site/static"},
      {error_log, "./log/inets.log"},
      {modules, [nitrogen_inets]},

      {mime_types, [
          {"css", "text/css"},
          {"js", "text/javascript"},
          {"html", "text/html"}
      ]}
  ].

```

   * `bind_address` (IP Address as a 4-tuple) - Note that the `bind_address`
    for Inets is different than for the rest of the servers in that it expects the
    address to be in the form of a 4-tuple for example, instead of specifying the
    string (ie `"12.34.56.78"`, you would specify `{12,34,56,67}`).

   * `error_log` (String) - The name of the file to store the inets logs.

   * `modules` (List of module names) - For each request, Erlang will attempt
    to call `ModuleName:do/1` for each specified module. Typically, we just put in
    the atom `nitrogen_inets` as that's the default Nitrogen entry point for inets.

   * `mime_types` ([{Extension,Mimetype},...]) - This is simply a list of the
    Mime Types you wish to support along with the extensions that trigger those
    mime types. By default, it supports css, javascript, and html files. More types
    will have to be added by the user.


### Mochiweb: etc/mochiweb.config

  [Mochiweb](http://github.com/mochi/mochiweb) is a webserver written by Bob
  Ippolito. It's a very lightweight webserver and very easy to configure.

  The default configuration file for Mochiweb provided by Nitrogen is as follows:

```erlang

  [{mochiweb, [
      {bind_address, "0.0.0.0"},
      {port, 8000},
      {server_name, nitrogen},
      {document_root, "./site/static"},

      %% Max Request size of 25MB. While this is a mochiweb env_var,
      %% it's actually only used in simple_bridge
      {max_request_size, 26214400}
  ]}].

```

   * `max_request_size` (Integer) - Tells Mochiweb (in particular, it tells
    SimpleBridge) what the maximum request size to be honored. This is in bytes.
    The current default is 25 MB maximum request size.

### Webmachine: etc/webmachine.config

  [Webmachine](http://wiki.basho.com/Webmachine.html) is a web server written
  by [Basho](http://basho.com) (the makers of Riak), and it provides functions
  to specify detailed dispatch rules.

  The basic config file provided for Webmachine is very simple and minimal (it's
  basically the same as the one for cowboy)

```erlang
  [{webmachine, [
      {bind_address, "0.0.0.0"},
      {port, 8000},
      {document_root, "./site/static"},
      {server_name,nitrogen},
      {static_paths, ["js/","images/","css/","nitrogen/"]}

  ]}].

```

   * `static_paths` (List of Strings) - Used to determine if a requested resource should be
    handled by Nitrogen, or if it should just be immediately served directly by
    Webmachine. (Default: `["js/","images/","css/","nitrogen/"]`)

    **Note 1:** This is relative to the `document_root` above. So requests for `js/`
    will be served from `./site/static/js/` (using the default above).

    **Note 2:** it is **strongly** recommended to catch static files with the
    `static_paths` setting. simple_bridge does not serve large static files in an
    optimal way (it loads the files into memory completely before sending).

##### More Webmachine Dispatch Rules: site/src/nitrogen_sup.erl

  Webmachine also provides a dispatch table to allow you to specify how requests
  are handled (beyond the basics covered by Nitrogen and the configuration
  above). If you're interested in diving into that, check out the
  `site/src/nitrogen_sup.erl` file in your Nitrogen installation.

### Yaws: etc/yaws.config

  [Yaws](yaws.hyber.md) is a high performance webserver created by
  [Claes Wikstrom](https://github.com/klacke) and is a unique addition to the
  Nitrogen's supported webserver line-up because it's one of the few that uses
  Apache-style configuration instead of the more usual Erlang proplist config 
  files.

##### etc/yaws.config

  This file is just tells Yaws where to load the actual configuration file,
  which you can probably deduce.

```erlang
  [{yaws, [
      {conf, "./etc/yaws.conf"}
  ]}].

```

##### etc/yaws.conf

```bash
  logdir = ./log
  <server mydomain.org>
      port = 8000
      listen = 127.0.0.1

      #the static code to be served directly by yaws is found in ./site/static
      docroot = ./site/static

      # tell yaws to pass control to the nitrogen_yaws module
      # (specifically nitrogen_yaws:out/1) for all requests except for any request
      # that starts with "images/", "nitrogen/", "css/", or "/js".
      # Bear in mind, however, the caveat to this performance improvement:
      # this means that you cannot have any pages called "nitrogen_xxx" or "css_yyy" because
      # the yaws config will see the "exclude_paths" rule below and completely ignore nitrogen.
      # Should you wish to have yaws handle any more static files, for example, if you added
      # a videos directory in site/static/, you can simply add "videos" to the end of the list
      # Ex: appmods = </, nitrogen_yaws exclude_paths images nitrogen css js videos>
      appmods = </, nitrogen_yaws exclude_paths images nitrogen css js>
  </server>

```

  You can find the [complete documentation for the yaws.conf file](http://yaws.hyber.org/yman.yaws?page=yaws.conf)
  on the official website, but for the sake of convenience, here's the a brief
  description of the default one provided by Nitrogen.

 *  `logdir` :: tells where to store the Yaws log files
    
 *  `<server mydomain.org> [...] </server>` :: Defines a virtual server. For use
      with Nitrogen, we recommend only specifying one. `mydomain.org` in our example
      is simply the name of the virtual server, and is not used for anything beyond
      a naming scheme.
    
 *  `port` :: The port to listen on.
    
 *  `listen` :: Which IP address to listen on.
    
 *  `docroot` :: The location of the static files relative to the Nitrogen installation
    
 *  `appmods ` </, nitrogen_yaws exclude_paths images nitrogen css js>= :: While
      quite long and dense with information, this configuration setting tells Yaws to
      send all requests to the Erlang module `nitrogen_yaws`, except for any requests
      that start with /images, /nitrogen, /css, or /js, which will instead be
      handled by Yaws directly.
