# Spork

Various Janet utility modules. Spork aims to be grab bag of useful Janet functionality that
does not belong in the core library.

To use all features mentioned below in one program you can do `(use spork)` to
import them. When you need only part of the functionality, please use `import`
as seen in the example for the part.


## Formatting

Provides a way to format Janet code strings and files.

### Strings

```
(import spork/fmt)

(fmt/format "(def     a\n 3 )")  => @"(def a\n  3)\n"
```

### Files

```
(import spork/fmt)

(fmt/format-file "main.janet")
```

## Message Protocol

Provide a symmetric way to send and receive seqential messages over a networked stream.
Useful for building more complicated application level protocols on top of TCP.

```
(import spork/msg)

(def stream (net/connect "http://example.com" "1234"))

(def send (msg/make-send stream))
(def recv (msg/make-recv stream))

(send "blob1")
(def blob-respose (recv))
```

## Networked REPL

Launch a networked REPL server on one machine and connect to it from another machine or process.

### Server
```
(import spork/netrepl)

(def some-def 10)

# Serve a repl into the current environment (some-def will be visible)
(netrepl/server "127.0.0.1" "9000" (fiber/getenv (fiber/current)))
```

### Client
```
(import spork/netrepl)

# Starts a nice terminal repl.
(netrepl/client "127.0.0.1" "9000" "bob")
```

## RPC Protocol

A simple remote procedure call tool for Janet.

### Server
```
(import spork/rpc)

(def functions
  @{:print (fn [self x] (print "remote print: " x))
    :add (fn [self & xs] (sum xs))})

(rpc/server functions "127.0.0.1" "9001")
```

### Client
```
(import spork/rpc)

(def c (rpc/client "127.0.0.1" "9001" "joe"))

# Will print on server
(:print c "Hello from client!")

(:add c 1 3 5 7 9) # -> 25

# Close the underlying connection
(:close c)
```

## Path

Simple path manipulation module for Janet. Supports manipulation both
windows and posix paths on any platform, and provides functions that
work according to the current host platform.

All functions have three forms, under `path`, `path/win`, and `path/posix`. The
prefix
indicates which type of path the function manipulates.

```clojure
(import spork/path)

# Examples for a non-windows system, use path/win/ for windows and
# path/posix/ for posix.

(path/ext "my/long/path.txt") # -> ".txt"
path/sep # -> "/" on posix, "\\" on windows
path/delim # -> ":" on posix, ";" on windows
(path/basename "some/path.txt") # -> "path.txt"
(path/dirname "some/path.txt") # -> "some/"
(path/parts "some/path/file.txt") # -> ["some" "path" "file.txt"]
(path/normalize "some/.././thing/file.txt") # -> "thing/file.txt"
(path/join "some/path" "../thing/file.txt") # -> "some/thing/file.txt"
(path/abspath? "/home/blah") # -> true
(path/abspath "file.txt") # -> "/home/me/cwd/file.txt"

```
## argparse

A moderately opinionated argument parser for
[janet](https://janet-lang.org). Use this for writing
CLI scripts that need to have UNIX style switches and
options.

### Sample

```clojure
#!/usr/bin/env janet

(import spork/argparse :prefix "")

(def argparse-params
  ["A simple CLI tool. An example to show the capabilities of argparse."
   "debug" {:kind :flag
            :short "d"
            :help "Set debug mode."}
   "verbose" {:kind :multi
              :short "v"
              :help "Print debug information to stdout."}
   "key" {:kind :option
          :short "k"
          :help "An API key for getting stuff from a server."
          :required true}
   "expr" {:kind :accumulate
           :short "e"
           :help "Search for all patterns given."}
   "thing" {:kind :option
            :help "Some option?"
            :default "123"}])

(let [res (argparse ;argparse-params)]
  (unless res
    (os/exit 1))
  (pp res))
```

### Usage

Call `argparse/argparse` to attempt to parse the command line args
(available at `(dyn :args)`).

The first argument should be a description to be displayed as help
text.

All subsequent options should be alternating keys and values where the
keys are options to accept and the values are definitions of each option.

To accept positional arguments, include a definition for the special
value `:default`. For instance, to gather all positional arguments
into an array, include `:default {:kind :accumulate}` in your
arguments to `argparse`.

Run `(doc argparse/argparse)` after importing for more information.

## Temple

HTML templates for Janet.

Simplified version of Mendoza's template system that is cleaner and easier to use.
Templates can be used recursively, and output is printed via `print`, so goes to
`(dyn :out)`.

Expands on the mendoza templates with the `{-` ... `-}` brackets, which do non-escaped
substitution, so temple can be used for formats besides HTML.
Also exposes the `escape` function inside templates for HTML escaping
if you want to manually print to template output.

### Example

#### foo.temple

```
{$ (def n 20) # Run at template compile time $}
<html>
  <body>
    {{ (string/repeat "<>" n) # HTML escaped }}
    <ul>
      {% (each x (range n) (print "<li>" x " " (args :a) "</li>")) # No auto-print %}
    </ul>
    {- (string/repeat "<span>1</span>" n) # Not HTML escaped -}
  </body>
</html>
```

### main.janet

```
(import temple)
(temple/add-loader)

(import ./foo :as foo)
(foo/render :a "hello")
```

There is one more involved example in the (/janet-lang/spork/examples/temple/). You
can runnit with `janet  examples/temple/example.janet`.

## Test

This module contains a simple test helper when you do not need a specialized
library.

### assert

Modified version of assert, with some nice error handling.

```clojure
(test/assert false "How is this?")
# => ✘ How is this?
(test/assert true "OK")
# => ✔true
```

### assert-not

Invert assert.

```clojure
(test/assert-not false "OK")
# => ✔true
```

### assert-error

Test passes if forms throw errors.

```clojure
(test/assert-error "To err is natural" (error "Bad"))
# => ✔true
```

### assert-no-error

Test passes if forms throw errors.

```clojure
(test/assert-no-error "To not err is desired" (do "Good"))
# => ✔true
```

### start-suite

Starts test suite, which counts all and passed tests.

### end-suite

Ends test suite and print summary.

### All together

Example of simple test suite.

```clojure
(import spork/test)

(test/start-suite 0)

(test/assert true "is always true")
(test/assert-not false "is always false")
(test/assert-error "To err is natural" (error "Bad"))
(test/assert-no-error "To not err is desired" (do "Good"))

(test/end-suite)

# =>

Test suite 0 finished in 0.000 soconds
4 of 4 tests passed.

```

### timeit

Time code execution using os/clock, and print the result.
Returns the value of the timed expression.

```
repl> (misc/timeit (sum (seq [i :range [1 1000000]] (math/sqrt i))))
Elapsed time: 0.0718288 seconds
6.66666e+08
```

### capture-stdout

Runs the form and captures stdout. Returns tuple with result and captured
stdout in string.

```clojure
(capture-stdout
  (do
    (print "Interesting output")
    true))
# => (true "Interesting output")
```

### supress-stdout

Runs the form, but supresses its stdout.

```clojure
(suppress-stdout (print "Hello world!"))
# => nil
```

## Misc

### Dedent

Remove indentation after concatenating the arguments.

```clojure
(misc/dedent ``
      ho
        hoho
          hohoho
``))))) => "ho\n  hoho\n    hohoho"
```

### set*

Allow parallel mutation of multiple mutable variables.  (All right
hand sides are computed before setting the left hand sides.)

```
# you can use it with vars
(var a 2)
(var b 3)
(misc/set* [a b] [b (+ a b)])
[a b] => [3 5]

# or you can use it with arrays, for example:
(def x @[2 3])
(misc/set* [[x 0] [x 1]] [(in x 1) (+ (in x 0) (in x 1))])
x => @[3 5]
```

## Installation

```
[sudo] jpm install https://github.com/janet-lang/spork.git
```
