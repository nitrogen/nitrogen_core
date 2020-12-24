<!-- dash: Content Security Policy | Guide | ##:Section -->

# Content Security Policy

## Overview

[Content Security Policy](https://content-security-policy.com) (CSP)
is an HTTP response header that helps reduce XSS risks by declaring
which dynamic resources are allowed to load.  Nitrogen supports CSP in
two ways:

  * Producing an appropriate HTTP header from a configuration.

  * Creating and exposing a nonce values for use in templates.

## Content-Security-Policy Header

If the configuration key `content_security_policy` is present in the
config (specifically via `wf:config(content_security_policy)`) a
header will be produced.  The format of the configuration is:

``` erlang
{nitrogen_core,
  [{content_security_policy,
    [
      {default_src, [none]},
      {script_src, [self, nonce, unsafe_eval]},
      {style_src, [self, "*.fontawesome.com"]}
    ]}
  ]}.

```

Each key in the proplist represents a CSP domain (e.g., "default-src",
"script-src", "style-src"); the value of each element is a list of
permitted sources.

Domains may either be atoms (as shown, with underscores instead of
hyphens) or strings (retaining the hyphens documented in the CSP
specification).

Sources might be strings ("*.fontawesome.com") or atoms (self).
Strings are left untouched by the CSP header constructor; atoms are
translated into single-quoted strings.  The following atoms are
available:

  * none
  * self
  * data
  * https
  * unsafe_inline
  * unsafe_eval
  * strict_dynamic
  * unsafe_hashes
  * nonce

If the `content_security_policy` key is missing, no CSP header will be
generated.  (Although a nonce value will still be generated, see
below.)

## Nonce Values

Each first_request generates a 12 character nonce value which becomes
the `script_nonce` attribute of the `#context` record.  This value is
made available via `wf:script_nonce/0` and is appropriate for use in
templates, e.g.,

``` html
<script nonce="[[[wf:script_nonce()]]]">
  [[[script]]]
</script>
```

If a `content_security_policy` references `nonce` in its source list,
the value created for the header is `"nonce-" ++ wf:script_nonce()`.

## Note

Details on how to configure an appropriate CSP ruleset is outside the
scope of this document (and can be non-trivial).  Two Nitrogen
specific hints:

  * `script_src` will most likely require `unsafe_eval` due to the way
    Nitrogen handles browser communication; and,

  * `connect_src` will most likely require some specification for the
    websocket.

## References

- [Mozilla Developer Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP)
- [content-security-policy.com](https://content-security-policy.com)
