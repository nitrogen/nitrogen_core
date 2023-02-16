<!-- dash: Configuration | Guide | ##:Section -->


# Configuration Options

## Configuration Overview

  Nitrogen configuration is done in a handful of different configuration files,
  and also varies slightly between backend servers. All of these files are found
  in the `etc/` directory of your Nitrogen installation.  Below we will go though
  the configuration options provided within each.

  It's worth noting that stock Nitrogen has a
  [config combiner script](https://github.com/nitrogen/nitrogen/blob/rebar3/templates/common/etc/assemble_config.escript)
  that [runs before compilation](https://github.com/nitrogen/nitrogen/blob/rebar3/templates/backends/sources/rebar.base.config#L18).
  This allows you to break configuration files up into multiple files, rather
  than the standard Erlang approach of having one big app.config file with all
  configs. It provides a minor convenience, but is not a standard thing in
  Erlang.

## Univeral Configuration Files (configs available in all Nitrogen intallations)

### `etc/vm.args`

  This is the file that sends sends options to the Erlang Virtual Machine.
  Basically, any of the usual
  [erl startup parameters](http://www.erlang.org/doc/man/erl.html) will work
  here.  You'll define some standard options here, such as the node name,
  (`-name`), erlang cookie (`-cookie`), some miscellaneous environment options,
  and of course, to tell the system to launch the Nitrogen application upon
  startup.

  Here is the complete text of the default vm.args:

```bash
  ## Name of the Nitrogen node
  -name nitrogen@127.0.0.1

  ## Cookie for distributed erlang
  -setcookie SOMERANDOMSTRING1234

  ## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive
  ## (Disabled by default..use with caution!)
  ##-heart

  ## Enable kernel poll and a few async threads
  +K true
  +A 5

  ## Increase number of concurrent ports/sockets
  -env ERL_MAX_PORTS 4096

  ## Tweak GC to run more often
  -env ERL_FULLSWEEP_AFTER 10

```

### `etc/app.config`

This file contains the configuration options for the Nitrogen application
itself. It uses the standard Erlang config syntax: (a proplist of
`{applicationName, AppOptions}`).

For the `nitrogen_core` application, the following `AppOptions` are recognized:

  * `signkey` (String) - `signkey` is the term that Nitrogen usees to "sign"
	the state information that's sent to the client, and then resent back to
	the server with requests.  While Nitrogen's state info is not encrypted,
	this is used to help verify that the term generated is actually generated
	by Nitrogen, to prevent client-side tampering with the state. (the default
	is to base it on the Erlang cookie)

  * `session_timeout` (integer) - `session_timeout` is the number of minutes
    a session can be idle before Nitrogen expunges it from memory

  * `cookie_name` (string) - `cookie_name` is the name of the HTTP Cookie to
    be used as the sesson tracking cookie. This is not to be confused with the
    Erlang Cookie, which is defined in the vm.args file above.

  * `module_prefix` (string) - This provides the user with the ability to route
	all Nitrogen web requests to modules that all share the same prefix. For
	example, if `module_prefix` was set to "abc", then a request for `/mypage`
	would be directed to the module `abc_mypage`.

  * `module_aliases` (proplist) - This provides the template system with
    a default list of module aliases to use within Nitrogen templates. (See
    the [Template Element](template.md) for more details).

  * `application_config_key` (atom or [atom]) - This provides the
    default config handler an additional `applicationName` to search
    for `AppOptions`.  For example, if `application_config_key` is set
    to `myapp`, the config handler will search the `myapp`,
    `nitrogen_core`, and `nitrogen` namespaces (in that order) for the
    supplied configuration key.  Multiple applicationNames may be
    specified.

  * `application_config_fn` (function) - This specifies a custom
    function to the default config handler.  The function specified
    will have this spec:

    ```erlang
    config_fn(Key::any(), DefaultValue::any()) -> {ok, Value} | any().
    ```
    and will be called before searching `app.config` for a value.

    If `{ok, Value}` is returned, processing stops and `Value` is
    returned; any other return value will cause the default config
    handler to continue processing.

  * `test_browsers` (list of strings) - If you will employ Nitrogen's test
    system, this provides a list of browsers to execute to test.

  * `tests` - (list of strings) - If you are running any tests, these are the
    page URLs to test in the specified browser(s).  An example of this might
    be `["/mypage?test=1", "/some/other/page/test"]`.


Also typically defined in the app.config file here is the [SASL
Configuration](http://www.erlang.org/doc/man/sasl_app.html), the
default of which for Nitrogen can be seen here:


```erlang
    {sasl, [
        {sasl_error_logger, {file, "log/sasl-error.log"}},
        {errlog_type, error},
        {error_logger_mf_dir, "log/sasl"},      % Log directory
        {error_logger_mf_maxbytes, 10485760},   % 10 MB max file size
        {error_logger_mf_maxfiles, 5}           % 5 files max
    ]}

```

### `etc/simple_bridge.config`

  This file contains the configuration for SimpleBridge, Nitrogen's
  abstraction layer to each server.  When you generate your initial Nitrogen
  release with `rel_cowboy`, `slim_yaws`, etc., Nitrogen will select the
  correct backend in this file for you.

  * `backend` (*Atom* - cowboy | inets | mochiweb | webmachine | yaws) -
    This defines which backend SimpleBridge will use.  It's set up by
    Nitrogen's release generation process.  Note that if you change this value,
    you'll need to make sure you copy in the
    [appropriate dependencies](https://github.com/nitrogen/simple_bridge/tree/master/rebar_deps)
    into the rebar.config file, as this toggle doesn't do it
    automatically.

  * `handler` (Module Name) - This is the name of the SimpleBridge
    handler that will deal with the request.  In the case of Nitrogen, this
    should almost always be the atom `'nitrogen'`, which is a module in
    `nitrogen_core`.

  * `bind_address` (String) - The string or Erlang tuple of the IP
    address to bind.  If set to "0.0.0.0", `{0,0,0,0}` or left blank, it'll bind
    to all available addresses. (Default: `"0.0.0.0"` )

  * `port` (Number) - The port number to bind. (Default: `8000`)

    **About Ports and Linux/Unix**: While port 80 is the standard HTTP port, port 80 is a
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
			sudo setcap cap_net_bind_service+ep /path/to/erts-5.9.2/bin/beam
			sudo setcap cap_net_bind_service+ep /path/to/erts-5.9.2/bin/beam.smp
    ```

    This will give the `beam` and `beam.smp` programs privileges to bind to
    privileged ports (ports under 1024).

  * `server_name` (atom or string) - What to name the server. (Default: `nitrogen`)

  * `document_root` (String) - The root of the location of static
    resources (ie, stylesheets, javascript files, images, etc). This will be
    passed to simple_bridge for the serving of static files. (Default:
    `"./site/static"` )

    **Note:** this is relative to the root of the Nitrogen installation.

  * `static_paths` (List of Strings) - The list of request paths which
    will automatically be handled by the static handler.  Generally, this will
    be a list of directories (or files) within `document_root` provided above.

    Consider the that your project as a  `priv/static` directory has the
    following files:

    `favicon.ico` (your site's favicon), `js/` (a directory containing
    javascript files), `css/` (a directory containing CSS rules).

    You should have the following in your config:

	```erlang
	{document_root, "priv/static"},
	{static_paths, "favicon.ico", "js/", "css/", "nitrogen/", "images/"},
	```

	This will ensure that requests to (for example),
    `"http://yoursite.com/js/my_javascript.js"` gets handled by the server's static
    handler, and will be served from "priv/static/js/my_javascript.js"

    Full configuration can be seen in Nitrogen's included
    [simple_bridge.config](https://github.com/nitrogen/nitrogen/blob/rebar3/templates/common/etc/simple_bridge.config)
    or in SimpleBridge's [sample config.](https://github.com/nitrogen/simple_bridge/blob/master/etc/simple_bridge.config)


### `plugins.config`

  The Nitrogen plugin system is capable of automatically including plugin
  header files, along with importing static web resources (javascript files,
  images, etc).  This can be done, and can be configured to work with just
  about **any** Nitrogen configuration, including non-standard configurations,
  such as manually importing Nitrogen as a dependency.

  The plugins.config file contains information for how Nitrogen will import
  plugins.

  It's a standard Erlang config file that takes three arguments.

  * `plugins_hrl` (path string) - Tell the Nitrogen plugin importer where
    to put the generated plugins.hrl file for the purposes of including plugin
    elements into your application. (default: `"./include/plugins.hrl"`)

  * `static_dir` (path string) - Tell the plugin system where you wish to
    put your plugins' static resources. (default: `"./priv/static/plugins"`)

  * `copy_mode` (copy|link) - Tell the plugins system how to include any
    static resources.  `copy` will copy the entire contents of your plugins'
    static directories, while `link` will merely create a symlink. If you work
    primarily with Linux or OSX, you can probably get away with using `link`,
    while if you use Windows, you should stick with `copy`. (default: `copy`).

[Here's the complete text of the default plugins.config](https://github.com/nitrogen/nitrogen/blob/rebar3/templates/common/plugins.config).

#### More about Plugins

  The plugin system has its own complete documentation along with a sample
  plugin stub for creating your own plugins.

  We strongly advise reading the [Plugin Documentation](plugins.md).

### Notable mention: rebar.config

  The standard for Erlang distribution and building is the use of
  [rebar3](http://rebar3.org). The standard Nitrogen installation will
  automatically build a rebar3 binary for your platform so you don't have to
  think about it too much.

  The file
  [rebar.config](https://github.com/nitrogen/nitrogen/blob/rebar3/templates/backends/sources/rebar.base.config)
  controls things like dependencies and build instructions, including options for
  building releases with [relx](https://github.com/erlware/relx), rather than the
  older [reltool](https://www.erlang.org/doc/man/reltool.html) that comes built
  into Erlang. It also includes integration with [hex.pm](https://hex.pm),
  which has become the standard repository for Erlang and Elixir package
  management.

  Then run `make` from the root of your Nitrogen installation. This will download
  the new dependencies and install them into the `lib` directory of your
  installation.


## Server-specific Configuration (Cowboy, Yaws, etc)

  If you're running an older version of Nitrogen (pre-2.3) or you're running a
  Nitrogen without using the standard simple_bridge config, you can check out
  the [server-specific configuration options](old_config.md) (no longer
  maintained), otherwise, you'll have to look at the server's official
  documentation.

## Additional Configuration

### Smart Extensions

   See [Smart Extensions Documentation](smart_extension.md)

### nginx - A lightweight reverse proxy

  [Nginx](http://wiki.nginx.org/Main) is high performance, lightweight web
  server and reverse proxy that is commonly used for load balancing, rewrite
  rules, SSL certificates, and more.

  Here's a sample configuration (this assumes a standard Ubuntu configuration):

##### /etc/nginx/nginx.conf

```php
  user www-data;
  worker_processes  1;

  error_log  /var/log/nginx/error.log;
  pid        /var/run/nginx.pid;

  events {
              worker_connections  4096;
  }

  http {
          include       /etc/nginx/mime.types;
          default_type  application/octet-stream;

          access_log      /var/log/nginx/access.log;

          sendfile        on;

          keepalive_timeout  10;
          tcp_nodelay        on;

          gzip  on;

          proxy_set_header X-Forwarded-Host $host;
          proxy_set_header X-Forwarded-Server $host;
          proxy_set_header Host $host;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;


          include /etc/nginx/conf.d/*.conf;
          include /etc/nginx/sites-enabled/*;
  }

```

##### Non-SSL Sample: /etc/nginx/sites-enabled/my_site

```php
  server {
          listen   80;
          server_name  mysite.com www.mysite.com;
          access_log  /var/log/nginx/mysite.com.access.log;
          location / {
                  proxy_pass http://127.0.0.1:8000;
          }
  }

```

##### SSL-Only Sample: /etc/nginx/sites-enabled/my_secure_site

  This configuration will serve only SSL. It will redirect all requests
  from the HTTP port (port 80) to the HTTPS port (port 443) and load the certificates

```php
  # My config for a site that I only want serving SSL content.
  server {
          listen   80;

          server_name www.mysite.com, mysite.com;
          access_log  /var/log/nginx/mysite.com.access.log;

    # rewrite all requests to be SSL
          rewrite ^(.*) https://$host$1 permanent;
  }

  server {
          listen 443;
          server_name mysite.com www.mysite.com
          access_log /var/log/nginx/mysite.ssl.access.log;

          ssl on;

          ssl_certificate ssl/mysite/mysite.com.crt;
          ssl_certificate_key ssl/mysite/mysite.com.key;
          ssl_client_certificate ssl/mysite/ca.crt;

          location / {
          # This installation is running on port 8000, as you can plainly see.
            proxy_pass http://127.0.0.1:8000;
          }
  }

```
