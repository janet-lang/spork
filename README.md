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

## Misc

### Dedent

Remove indentation after concatenating the arguments.

```clojure
(misc/dedent ```
      ho
        hoho
          hohoho
```) => "ho\n  hoho\n    hohoho"
```

## Installation

```
[sudo] jpm install https://github.com/janet-lang/spork.git
```

