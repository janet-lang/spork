(import ../spork/test)

(test/start-suite)
# test/assert
(assert (test/assert true "assert"))

# test/assert-not
(assert (test/assert-not false "assert-not"))

# test/assert-error
(assert (test/assert-error "assert-error" (error "Bad")))

# test/assert-no-error
(assert (test/assert-no-error "assert-no-error" "Good"))

# test/capture-stdout
(test/assert (= [true "Output\n"] (test/capture-stdout (print "Output") true)) "capture stdout")

# test/capture-stderr
(test/assert (= [true "Output\n"]
                (test/capture-stderr (eprint "Output") true)) "capture stderr")

# test/timeit
(do
  (def [result output]
    (test/capture-stdout
      (test/timeit (loop [i :range [1 100000]] i) "foo:")))
  (test/assert (nil? result))
  (def m (peg/match '(* "foo: " (<- (some :S)) " seconds\n" -1) output))
  (test/assert (truthy? m) "timeit -- invalid output")
  (test/assert (scan-number (in m 0)) "timeit -- invalid number of seconds"))

(do
  (def [result output]
    (test/capture-stdout
      (test/timeit "someresult")))
  (test/assert (= result "someresult") "timeit2 -- invalid return")
  (def m (peg/match '(* "Elapsed time: " (<- (some :S)) " seconds\n" -1) output))
  (test/assert (truthy? m) "timeit2 -- invalid output")
  (test/assert (scan-number (in m 0)) "timeit2 -- invalid number of seconds"))

# test/capture-stdout
(test/assert
  (= [nil "\nRunning test suite 666 tests...\n\n\e[32m\xE2\x9C\x94\e[0m\n\nTest suite 666 finished in 0.000 soconds\n13 of 13 tests passed.\n\n"])
  (test/capture-stdout
    (test/start-suite 666)
    (test/assert true "true")
    (test/end-suite)))

# test/suppress-stdout
(test/assert
  (= [nil ""]
     (test/capture-stdout
       (test/suppress-stdout (print "Hello world!"))))
  "suppress-stdout")

# test/suppress-stdout
(test/assert
  (= [nil ""]
     (test/capture-stderr
       (test/suppress-stderr (print "Hello world!"))))
  "suppress-stderr")

(test/assert-docs "/spork/test")
(test/assert-docs "/spork/argparse")
(test/assert-docs "/spork/fmt")
(test/assert-docs "/spork/generators")
(test/assert-docs "/spork/getline")
(test/assert-docs "/spork/htmlgen")
(test/assert-docs "/spork/http")
(test/assert-docs "/spork/httpf")
(test/assert-docs "/spork/misc")
(test/assert-docs "/spork/msg")
(test/assert-docs "/spork/netrepl")
(test/assert-docs "/spork/path")
(test/assert-docs "/spork/regex")
(test/assert-docs "/spork/rpc")
(test/assert-docs "/spork/schema")
(test/assert-docs "/spork/sh")
(test/assert-docs "/spork/tasker")
(test/assert-docs "/spork/temple")

(test/assert-docs "spork/json")
(test/assert-docs "spork/tarray")
(test/assert-docs "spork/rawterm")
(test/assert-docs "spork/utf8")

(test/end-suite)
