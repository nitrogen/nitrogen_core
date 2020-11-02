

## Redirect Action Element

	The `#redirect{}` action will redirect the page to the provided URL.

### Attributes

   * `url` (string) - The URL to redirect the page to.

	*  login - (/string | boolean()) :: If set to a URL (string or binary), will
	  treat the redirect as if it were called with `wf:redirect_to_login/2`. If
	  set to `true`, will treat it as if it was called with
	  `wf:redirect_to_login/1`. (Default: `false`)
