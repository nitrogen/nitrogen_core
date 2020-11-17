

## Gravatar Element - #gravatar {}

  The gravatar element displays an avatar that is associated with an
  email address.

  To use, create a gravatar element on your Nitrogen page, and set the
  email, size and rating attributes.

### Usage

```erlang
   #gravatar { email="email@address.com"
     size="80", 
     rating="g", 
     default="identicon" 
   }

```

### Attributes

   * `email` (string) - Email of the user whose icon will be displayed.

   * `size` (string) - Size of the gravatar image that is displayed.

   * `rating` (string) - g, pg, r, x.

   * `default` (string) - monsterid, wavatar or identicon.

### See Also

 *  [image element](./image.md)
 *  [Simple Controls Demos](http://nitrogenproject.com/demos/simplecontrols)
