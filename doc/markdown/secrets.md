<!-- dash: Secrets Management | Guide | ##:Section -->

# Secrets Management

## Overview

Nitrogen's default secrets handler relies on an application called
[`nitro_secret`](<https://github.com/nitrogen/nitro_secret>]. The default
behavior in Nitrogen is very basic, and reads a different config file from the
file system.

## How to use (with prose)

The location of the file is determined by the `secrets_filename` key in your
`app.config` (or the `nitro_secret.config` file). An example of this file can
be [found
here](https://github.com/nitrogen/nitro_secret/blob/master/priv/nitro_secret.config).

The secrets file itself is a basic Erlang config file [like
this](https://github.com/nitrogen/nitro_secret/blob/master/priv/sample_secrets.config).

The purpose of this particular simple secrets manager is so that you can read
secrets from the local machine rather than hard-coding or including a config
file in your Git repository.

## Exammple (with code)

### Storing your secrets

Our example app relies on a fictional API service called "Buttered Toast," for
which we have the API Key `bt-3g023posang0i45w4qwgv34-0edrih`. We need to be
able to get this API key in our app, but we don't want to include the API in
source control, so let's put it in our home directory.

Let's create this file here: `~/secrets/my_amazing_app_secrets.config`

And the contents of this file should be this:

```erlang
{buttered_toast, "bt-3g023posang0i45w4qwgv34-0edrih"}.
```

### Tell Nitrogen/nitro_secret where to find this file

Now with that saved, let's create, in our generated nitrogen app, this file:
`etc/nitro_secret.config` and the contents of this file will be this:

```erlang
[{nitro_secret, [
    {secrets_filename, "/home/YOUR_USERNAME/secrets/my_amazing_app_secrets.config"}
]}].
```

_(obviously, replace `YOUR_USERNAME` with your username)_

### Using the secret

Now, in your app's modules, if you need to retrieve the API key for "Buttered
Toast," you can call:

```erlang
wf:secret(buttered_toast)
```

This should return the string `"bt-3g023posang0i45w4qwgv34-0edrih"`

## Using other Secrets Services

If you wish to query a secrets service of your choice, the solution would be to
add your own [secret_handler](secret_handler.md). Some examples of external
secrets managers would be:

- [Bitwarden Secrets Manager](https://bitwarden.com/products/secrets-manager/)
- [OpenBao](https://openbao.org/)
- [Passbolt](https://www.passbolt.com)
- [Vault](https://developer.hashicorp.com/vault)
- [AWS Secrets Manager](https://aws.amazon.com/secrets-manager/)

The Nitrogen team is open to pull requests to add built-in support for any
common secrets manager you may wish to add.

## See Also

- [API: wf:secrets](api.html#wf_secrets)
- [secret_handler](secret_handler.md)
- [App: nitro_secret](https://github.com/nitrogen/nitro_secret)
