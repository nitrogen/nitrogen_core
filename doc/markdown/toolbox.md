<!-- dash: Toolbox | Guide | ##:Section -->

# The Nitrogen Toolbox

## Helpful libraries, utilities, and tools to make your development easier

### Nitrogen Plugins

* [The Official List of Nitrogen Plugins](https://github.com/nitrogen/nitrogen/wiki/Nitrogen-Plugins)

### Communication

* [Sparkler](https://github.com/choptastic/sparkler) - A simple Erlang
  application for sending email through [Sparkpost](https://sparkpost).
* [twilio_sms](https://github.com/choptastic/twilio_sms) - A simple Erlang app
  for sending text messages with [Twilio](https://www.twilio.com/)
* [gen_smtp](https://github.com/gen-smtp/gen_smtp) - A full-featured Email
  server in Erlang. Handle sending and receiving via SMTP directly.

### Databases

* [SQL Bridge](https://github.com/choptastic/sql_bridge) - A library and
  compatibility layer designed to work with Nitrogen to make it a little easier
  to interface with SQL databases. Currently supports MySQL and PostgreSQL.
  Planning to support SQLite as well.
* [MySQL/OTP](https://github.com/mysql-otp/mysql-otp) - The current best
  MySQL-specific adapter for Erlang to MySQL. SQL Bridge (above) uses MySQL/OTP
  as its adapter for MySQL.
* [epgsql](https://github.com/epgsql/epgsql) - The more-or-less official
  PostgreSQL adapter for Erlang. SQL Bridge (above) uses epgsql as its adapter
  to Postgres.
* [Riak](https://riak.com/) - A massively scalable Erlang-based NoSQL database
  originally built by Basho, and now maintained by [Erlang Solutions](https://erlang-solutions.com)
* [BossDB](https://github.com/ErlyORM/boss_db) - A DB-neutral ORM originally
  built into the [ChicagoBoss](http://chicagoboss.org) web framework.

### HTTP Clients

* [httpc](https://www.erlang.org/doc/man/httpc.html) - This is the HTTP client
  built into Erlang's Inets library. Very handy if you need something basic
  that doesn't require any dependencies.
* [ibrowse](https://github.com/cmullaparthi/ibrowse) - This is a more
  full-featured HTTP client in pure Erlang, and is very handy.
* [Gun](https://github.com/ninenines/gun) - A newer HTTP client written by the
  author of the mega-popular [Cowboy](https://github.com/ninenines/cowboy)
  Erlang webserver.

### Cloud

* [erls3](https://github.com/choptastic/erls3) - This is a fork of the
  [original erls3](https://github.com/cstar/erls3) project.  This forked
  version is maintained by the maintainer of Nitrogen.

### Graphics

* [erl_gm](https://github.com/nuex/erl_gm) - This is a wrapper for the
  [GraphicsMagick](http://www.graphicsmagick.org/) application for generating
  and changing images.
