# Spork

[![test](https://github.com/janet-lang/spork/actions/workflows/test.yml/badge.svg)](https://github.com/janet-lang/spork/actions/workflows/test.yml)

Various Janet utility modules. Spork aims to be grab bag of useful Janet functionality that
does not belong in the core library.

To use all features mentioned below in one program you can do `(use spork)` to
import them. When you need only part of the functionality, please use `import`
as seen in the example for the part.

## Build

```
janet -l ./bundle -e '(build)'
```

## Test

Spork must be installed to test properly.

```
janet --install .
janet -l ./bundle -e '(check)'
```

## Installation

As of Janet version 1.38.0, the normal Janet binary can install spork in the following way:

```
[sudo] janet --install .
```

This will install all spork modules to `$JANET_PATH` and all executable scripts to `$JANET_PATH/bin`.

For versions prior to 1.38.0, but with support for the bundle module:

```
[sudo] janet -e '(bundle/install ".")'
```

Or, finally, with JPM (legacy):

```
[sudo] jpm install spork
```

## Dependencies

Spork contains third-party dependencies in the `deps/` directory. All dependencies are MIT/X11 licensed, or public domain.
Licenses for individual components can be found along with the source code in the `deps/` directory.

## Documentation

Spork's documentation is written using [Mendoza](https://github.com/bakpakin/mendoza).
  The docs are most easily read by first building and then serving the .mdz files in `doc/` using Mendoza.
  You can then access the served static site using a browser.

- First, make sure you have [Janet](https://janet-lang.org/) and [jpm](https://janet-lang.org/docs/jpm.html) installed. See [the Janet docs](https://janet-lang.org/docs/index.html) for more information on this.
- Next, install Mendoza (to install globally, run `[sudo] jpm install mendoza`). For more information on Mendoza, see [the Mendoza project on GitHub](https://github.com/bakpakin/mendoza).
- Clone this repo locally (e.g. using `git clone https://github.com/janet-lang/spork.git`).
- From the spork project root (`cd spork` if you just cloned it) run `mdz build && mdz serve`.

While the Mendoza server process is running, you can navigate to http://localhost:8000 to view the spork docs as a static site.

