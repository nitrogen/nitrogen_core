# Documentation TODO

There is always room to add more documenation. So here is a quick list of
documentation I want to see added, in no particular order.

* How to add custom Elements
* How to add custom Actions
* How to add custom Mobile Elements
* How to add custom Validators
* Tips for debugging Nitrogen app
* Improvements to the guides for adding custom handlers
* Recommendations for contributing to Nitrogen
* Building a simple HTTP API in Nitrogen
* Using the RESTful elements
* Set up links from Elements and Actions to related Demos and back

## Desired Guides

These can be added as links to blog posts that describe the steps, if not made
as standard org-mode documentation. Most of these aren't Nitrogen-specific and
in some cases, even Erlang-specific, but useful questions that answer questions
commonly presented on Stackoverflow or the mailing lists.

* Integrating with common databases
* Working with JSON
* Working with dates
* Secure password hashing
* Amazon S3 integration
* Email integration
* Reverse Proxy configuration
* Working with HTML5 Canvas
* Builing a sample app that does something from the ground up

## Other things that are worth doing with documentation

### Docs converted to Markdown

While org-mode is nice, and is currently the method by which we make docs for
Nitrogen, getting the feel of org-mode, and compiling docs is a little goofy
for folks who don't use Emacs.  As a result, I'd like to one day convert docs
to [Markdown](http://en.wikipedia.org/wiki/Markdown) as it's a more universal
markup language, has a simpler syntax (IMO) than org-mode.

### Headers pulled out of the Docs

One of the side-effects of org-mode is that the documentation headers and menus
are embedded in each doc file.  I'd like to see the menu separated out of the
individual documentation files and inserted automatically by what would have to
be a new documentation compiler, converting Markdown to HTML.


## Contributing

If you want to write some guides, feel free to make pull requests, or if you
did so with a blog post or something, go ahead and make a pull request linking
to it.

Any questions, make a Github issue, or contact Jesse Gumm at
[@jessegumm](http://twitter.com/jessegumm) or by [email](mailto:gumm@sigma-star.com).
