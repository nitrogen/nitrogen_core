

## Email Link Element - #email_link{}

  The link element produces a clickable email link.

### Usage

```erlang
  #email_link { email="joe@mycompany.com" },

```

```erlang
   #email_link { email="samantha@yourcompany.com", text="Click to Mail me" }

```

```erlang
   #email_link { email="support@somecompany.com", body=#image { image="path/to/email_support.gif" } }

```

### Attributes

   * `email` (string) - The email address to mail to.

   * `text` (string) - The text to display.  If none is specified, will default to the email address provided.

   * `body` (Nitrogen elements) - Instead of text, specify one or more Nitrogen elements (such as an #image) to wrap in a link.

   * `html_encode` (boolean) - Set to true to safely html-encode the link's title.

### See Also

 *  [base element](./base.html)

 *  [link element](./link.html)

 
