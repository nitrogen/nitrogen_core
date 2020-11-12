
# Getting Started

## Common Questions

 *  [Is there a Nitrogen Tutorial?](0)
 *  [How do I create a new Nitrogen application?](100)
 *  [How do I add Nitrogen to an existing Erlang application?](200)
 *  [How do I start and stop my application?](300)
 *  [How do I change configuration settings?](400)
 *  [Where is the code for my website?](500)
 *  [How do I compile my code?](600)
 *  [How do I create new pages, custom elements, or custom actions?](700)
 *  [How do I make a mobile version?](800)
 *  [How do I work with Nitrogen Plugins?](900)
 *  [How do I upgrade to a new version of Nitrogen?](1000)
 *  [What do I do if I run into errors? How do I troubleshoot?](1100)

## Is there a Nitrogen Tutorial?

  Yes! You can [view the updated slides](tutorial.md) from Rusty's Nitrogen Tutorial at the
  Erlang User Conference 2010 here.
  
## How do I create a new Nitrogen application?

  Note: If you want to add Nitrogen to an existing application, see [this question](200)

  First, check the [Downloads](http://nitrogenproject.com/downloads) page to
  see if a pre-built package exists for your OS/architecture. If it does, then
  just download the package, unzip, and you are ready to start coding. (You
  don't even need to have Erlang installed, the Nitrogen binaries come with it
  pre-packaged.)

  If you don't see a package that will run on your computer, or you want to try
  running from the latest code in source control, then follow the steps below:

  Pull the latest source code from GitHub:

    git clone https://github.com/nitrogen/nitrogen

  Then run:

    make rel_inets

  This creates a completely self-contained starter application under
  `../myapp` that runs on Inets, Erlang's built in HTTP server.
  "Self-contained" here means that the `../myapp` directory contains
  everything you need to develop and run nitrogen, including the Erlang VM. To
  create a `.tar.gz` version of this code, exactly like you would find on the
  [Downloads](http://nitrogenproject.com/downloads) page, run:

    make package_inets

  You can also run Nitrogen with Mochiweb, Yaws, Cowboy, or Webmachine. Run one
  of the following make commands:

    make rel_cowboy
    make rel_mochiweb
    make rel_webmachine
    make rel_yaws

  **FreeBSD**: Compiling on FreeBSD requires using `gmake` instead of `make`,
  otherwise the steps are the same

  **Microsoft Windows**: There are special instructions for Windows users who
  wish to compile on Windows. Read about
  [Compiling
  Nitrogen on Windows](https://github.com/nitrogen/nitrogen/blob/master/rel/overlay/win/README.md)
 
  There are also "slim release" versions that can be built. These take
  advantage of a new feature introduced in Erlang's R15B02 release, which
  packages up an application, but does not include a full Erlang distribution
  (instead, it assumes the target system has Erlang installed already).

  You can create these "slim releases" by typing:
  
    make slim_cowboy
    make slim_inets
    make slim_mochiweb
    make slim_webmachine
    make slim_yaws

  All of the above commands can be provided with PREFIX or PROJECT variables to
  change the destination of the project, and the project directory name and
  node name. Note: it does not currently change the actual name of the Erlang
  application (the Application will still be called "nitrogen").

  For example:

    make slim_yaws PROJECT`my_awesome_yaws_app PREFIX`~/my_web_apps

  Will create a `my_web_apps/my_awesome_yaws_app` directory in your home
  directory.

## How do I add Nitrogen into an existing Erlang application?

  As of 2.2.0, adding Nitrogen to an existing Erlang application is simple.

  Clone Nitrogen somewhere on your machine:

    git clone git://github.com/nitrogen/nitrogen.git

  Then, from the root of your application's directory, run the `embed` script
  from the Nitrogen repo:

    /path/to/nitrogen/embed

  /Note: You must run this from your **application's** directory, **not** from the
  Nitrogen directory./

  /Note 2: This assumes your system has Perl installed/

  You can read the full writeup in
  [in this blog post](http://sigma-star.com/blog/post/embedding-nitrogen).

## How do I start/stop/manage Nitrogen?

  To start Nitrogen in the console

    bin/nitrogen console

  To start Nitrogen, type

    bin/nitrogen start

  To attach to a detached Nitrogen console

    bin/nitrogen attach

  To stop Nitrogen

    bin/nitrogen stop

## How do I change configuration settings?

  Read all about the [configuration options](config.md).

## Where is the code for my website?

  Your entire site's code can be found in `site/`.

  Within there, you'll find:
  
 *  `site/src` :: Your erlang code

 *  `site/static` :: Where static resources like CSS, Javascript, and Images Go

 *  `site/templates` :: Where your site's HTML templates go

 *  `site/ebin` :: Where the compiled BEAM files go.

  **If you're using a full release**, it's recommended to put the `site/` directory under source control.

  **If you're using a slim release**, you can put the entire application under source control.

## How do I compile my code?

  There are three different ways to compile code in Nitrogen:

 *  If you are **in the Erlang console** then run `sync:go().`

    This will start the `sync` process, which scans the filesystem
    for changes and automatically recompiles modules as they are changed.

    **Note:** sync will only recompile files that are changed /after/ sync was
    started. That means that if sync is not currently running, and you change a
    file, it will not detect that change.

 *  If **Nitrogen is running in the background** then run =./bin/dev
    compile=

    This connects to the running Nitrogen application and issues a
    `sync:go().` command.

 *  If **Nitrogen is stopped** then run `make`

    This calls `./rebar get-deps` to retrieve dependencies,
    `./do-plugins.escript` to process any plugins, and finally
    `./rebar compile`.

## How do I create new pages, custom elements, or custom actions?

  Nitrogen 2.0+ includes a developer tool to help you get started with a
  bare-bones page, element, or action. This tool uses the files found in
  `./site/.prototypes` as templates. If you wish, you can edit files in this
  directory to make them more suitable for your application.

 *  To create a new page at http://localhost/user/login, run:

    : ./bin/dev page user_login

 *  To create a new element called `#custom_element{}`, run:

    : ./bin/dev element custom_element

 *  To create a new action called `#custom_action{}`, run:

    : ./bin/dev action custom_action

  The code is placed under `./site/src`, `./site/src/elements`, or
  `./site/src/actions` for pages, elements, and actions,
  respectively. 

## How do I make a mobile version?

  You can read all about how to make mobile-enabled sites with Nitrogen by
  reading our
  [Mobile Integration Guide](http://nitrogenproject.com/doc/jquery_mobile_integration.html)

## How do I work with Nitrogen Plugins?

  Adding Plugins to a Nitrogen app is as simple as adding them as rebar dependencies.

```erlang
    {my_plugin, {git, "git://github.com/some_user/my_plugin.git", {branch, master}}}

```

  Then running:

    make

  You can read more detail about the Nitrogen plugin system its own section
  here [in the documentation](plugins.md).

  You can also play with creating your own Nitrogen plugins by checking out and
  modifying our
  [Sample Nitrogen Plugin](https://github.com/nitrogen/sample_nitrogen_plugin)

## How do I upgrade to a new version of Nitrogen?

  **Upgrading to 2.3**
    
  Following the instructions for upgrading to 2.2.2 will work except that
  websockets won't be available. For a more complete upgrade guide, see the
  [See the 2.3 upgrading guide](upgrade2.3.md)

  **Upgrading from 2.1 to any version up to 2.2.2**

  Edit your rebar.config file and make sure dependencies are pointing at the
  desired tag (ex: `{tag, "v2.2.2"}`) then run:

    make upgrade

  This will upgrade to the latest version of Nitrogen and it's dependencies.
  It will **not** upgrade to the latest Erlang Runtime System. If you wish to
  upgrade your version of ERTS, You'll want to follow the directions below for
  upgrading from pre-2.1 to 2.1+.

  **Note:** If you've run `make upgrade` and nothing seems to have worked,
  please check out the relevant question in the
  [Troubleshooting](troubleshooting.md) guide.

  **If you're running a version of Nitrogen pre 2.1 and want to upgrade to 2.1.0 and above:**
  
 *  Build or download the new Nitrogen environment for your architecture and
    preferred web server. Unzip to a new directory.

 *  Copy your existing `./etc` directory to the new build. This retains your
    configuration settings.

 *  Copy your existing `./site` directory to the new build, **except** for the
    files in `./site/static/nitrogen`. You will want to use the new version of
    any files in `./site/static/nitrogen`.

    This will update the Erlang version, the dependencies files, and
    all of the Javascript used by Nitrogen. 
    
    You will also need to update your code if there are any API level changes.

## What do I do if I run into errors? How do I troubleshoot? 

  See our [Troubleshooting Guide](troubleshooting.md)
