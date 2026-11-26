# Upgrading 2.1 to 2.2

Edit your rebar.config file and make sure dependencies are pointing at the
desired tag (ex: `{tag, "v2.2.2"}`) then run:

```bash
make upgrade
```

This will upgrade to the latest version of Nitrogen and it's dependencies.  It
will **not** upgrade to the latest Erlang Runtime System. If you wish to
upgrade your version of ERTS, You'll want to follow the directions for
[upgrading to 2.1](upgrade2.1).

**Note:** If you've run `make upgrade` and nothing seems to have worked, please
check out the relevant question in the [Troubleshooting](troubleshooting.md)
guide.
