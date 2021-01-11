# Spork

Various Janet utility modules. Spork aims to be grab bag of useful Janet functionality that
does not belong in the core library.


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

## Misc

### Dedent

Remove indentation after concatenating the arguments.

```
(misc/dedent ```
      ho
        hoho
          hohoho
```))))) => "ho\n  hoho\n    hohoho"

### timeit

Time code execution using os/clock, and print the result.
Returns the value of the timed expression.

```
repl> (misc/timeit (sum (seq [i :range [1 1000000]] (math/sqrt i))))
Elapsed time: 0.0718288 seconds
6.66666e+08
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

