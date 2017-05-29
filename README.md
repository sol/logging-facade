# logging-facade
A logging API for Haskell, inspired by
[slf4j](http://www.slf4j.org/).  Code that logs messages with
`logging-facade` doesn't have to depend on or make any decisions about the
logging backend that you want to use in the end. Choosing the logging backend
can be done at the top-most level -- e.g. in the `main` function of your
executable. Only at that point you have to actually depend on the libraries that
you need for whatever logging backend you're going for.

`logging-facade` itself only depends on libraries that ship with `ghc`.

`logging-facade` has a sensible default: logging to `stderr`. And there are
back ends for both [syslog](http://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html)
and [journald](https://en.wikipedia.org/wiki/Systemd#journald):

 * [logsink](https://github.com/sol/logsink#readme)
 * [logging-facade-journald](https://github.com/zalora/logging-facade-journald)
 * [logging-facade-syslog](http://hackage.haskell.org/package/logging-facade-syslog)
