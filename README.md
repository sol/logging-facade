# logging-facade
An experimental logging API for Haskell, inspired by [slf4j](http://www.slf4j.org/).
The idea is that code that logs messages with `logging-facade` doesn't have to depend on
or make any decisions about the logging backend that you want to use in the end. Choosing
the logging backend can be done at the top-most level -- e.g. in the `main` function of your
executable. Only at that point you have to actually import the libraries that you need for
whatever logging backend you're going for.

`logging-facade` itself only depends on libraries that ship with `ghc`.

`logging-facade` has a sensible default: logging to `stderr`. And there's a backend for
[journald](https://en.wikipedia.org/wiki/Systemd#journald):
[logging-facade-journald](https://github.com/zalora/logging-facade-journald).
