<!-- dash: REST | Guide | ###:Section -->


# REST Module

## Overview

  Nitrogen's default route handler gives you an easy way to add a no-nonsense
  RESTful handler to a page module. If you need to add a simple a RESTful
  interface without wanting to deal with Webmachine or Cowboy's restful
  handlers, and just use straight up Nitrogen, you can do this trivially.

  Simply stated, if you add `-behaviour(nitrogen_rest).` to a module, instead
  of looking for a `main()` function as the entry point, it will instead expect
  an entry point based on the HTTP request method used.  So a `GET` request
  will look for a `get()` function, and so on.

  Each Entry Point function expects a single argument, `PathInfo`, which is
  exactly yhe same information that would be retrieved from calling
  `wf:path_info()`.

### Request Methods Module Entry Points

   Commonly used Request methods and their corresponding entry points

 *  `GET` :: `get(PathInfo)`
 *  `POST` :: `post(PathInfo)`
 *  `PUT` :: `put(PathInfo)`
 *  `DELETE` :: `delete(PathInfo)`
   
   Less commonly used Request methods:

 *  `CONNECT` :: `connect(PathInfo)`
 *  `TRACE` :: `trace(PathInfo)`
 *  `OPTIONS` :: `options(PathInfo)`

   (You might notice the pattern here: the HTTP status code is made lower-case
   and converted to an atom, and that is the function called.

### Return values of entry points

   All the above functions are expected to return either a `Body`, which can be
   a typical Nitrogen body (with Nitrogen elements), or a 2-tuple,
   `{StatusCode, Body}`, where StatusCode is the HTTP status code of the
   return. If only a `Body` is returned, the `StatusCode` is the standard 200
   status code.


### See Also

 *  [Route Handler Docs](route.md)

 *  [REST Handler Demo](http://nitrogenproject.com/demos/rest_handler)
