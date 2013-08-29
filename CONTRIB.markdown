# Contributing to Nitrogen Web Framework

Contributions from the community are thoroughly welcome and pull requests are
fabulous! Those to make pull requests are even more fabulous! But there are
some things to consider when you've got the itch to make a pull request.

## Basic Guidelines

While we're okay with pull requests being more than one commit, we *prefer*
that pull requests be as minimal as possible (as few commits as possible,
possibly squashed with `git rebase -i`).

### Non-functioning commits should be rebased before being submitted

That said, we will be very picky about pull requests with non-functioning
commits (for example, commits with commit messages as "Work in Progress",
usually mean it doesn't work). Please squash those out with `git rebase`.

### Commit messages

Commit messages should try to be at most 50 characters, but if you go over a
few, it's not a big deal.  Keep the commit messages as concise as possible, and
if you need to add more elaboration, put it on the lines below the commit.
Please keep those lines under 80 characters.

## Bugfixes

### Have a simple (few-line) fix?

If you have a fix, especially if it's just a simple fix, put in a pull request.
No real need to submit an issue first.

### Have a more complicated fix?

If you have a complicated fix - something that touches lots of files, and may
or may not break API compatibility - go ahead and submit that pull request,
too. It's just going to take longer to pull and it might involve a discussion
of sorts.

### Have something that you think might need fixing, but aren't sure?

If you've encountered a bug, and think you'd like to fix it, but aren't sure if
it's even a bug (hey, it happens, software can be almost as ambiguous as
humans), submit an issue or send a message to the mailing list, or get your
hands dirty and make a fix as a proof-of-concept. Maybe it will be pulled
as-is, and maybe it'll require some tweaking before or after being pulled.

## Feature Requests

### Want to add something simple?

Want to add an attribute to an element or add an simple element of your own? Go
ahead and code it up and put in the pull request.

### Want to add something more complicated?

If you have something more mildly complicated, but aren't sure if it's
something that should be added to the `nitrogen_core` repo (like a complex
element, that does lots of fun things, and pulls in some Javascript files as
well), post an issue, or make a post to the mailing list.  We don't want to
fill the global namespace too badly with a pile of elements only a few will
use.

In some scenarios, this would make more sense as a [Nitrogen
Plugin](http://github.com/nitrogen/sample_nitrogen_plugin), rather than as
something core.

### Removing things

Removing things is obviously touchy - that's something that basically
necessitates breaking API.  Post an issue or to the mailing list about it.
That's something we can talk about for a future major version.

## Documentation

### Fuond a typo to fix?

Find a typo in our docs? Go ahead and fix that baby (and yes, the typo in the
header here is intentional)

### Want to add missing documentation?

Want to add missing documentation for, say, elements, actions, or validators?
Go ahead and add them and make a pull request. That sounds fantastic.

### Fix wording on docs?

Find some wording that doesn't quite get across what's intended? Go ahead and
revise it to what you think sounds clearer and make a pull request.  At the
very least, it will be considered and will start a conversation about what's
unclear in the documentation.


## Not sure where to start?

Not sure where to start, but want to contribute? Check out the [TODO
List](https://github.com/nitrogen/nitrogen/blob/master/TODO.markdown),
[Documentation TODO
List](https://github.com/nitrogen/nitrogen_core/blob/master/doc/org-mode/README.markdown),
or any outstanding issues on our github repos.

Or contact Jesse Gumm ([@jessegumm](http://twitter.com/jessegumm), or email the
[mailing list](https://groups.google.com/forum/#!forum/nitrogenweb)
