(import ../spork/test)

(test/start-suite 7)
# test/assert
(assert (test/assert true "assert"))

# test/assert-not
(assert (test/assert-not false "assert-not"))

# test/assert-error
(assert (test/assert-error "assert-error" (error "Bad")))

# test/assert-no-error
(assert (test/assert-no-error "assert-no-error" "Good"))

# test/capture-stdout
(test/assert (= [true "Output\n"] (test/capture-stdout (do (print "Output") true))) "capture output")

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
    (do
      (test/start-suite 666)
      (test/assert true "true")
      (test/end-suite))))

# test/suppress-stdout
(test/assert
  (= [nil ""]
     (test/capture-stdout
       (test/suppress-stdout (print "Hello world!"))))
  "suppress-stdout")

(test/end-suite)
