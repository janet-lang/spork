(use ./helper)
(import ../spork/misc)

(start-suite 1)

#misc/dedent
(assert (= (misc/dedent "  a\n    b\n   c\n     d") "a\n  b\n c\n   d") "dedent")

#misc/timeit
(defmacro- capture-stdout
  [form]
  (with-syms [buf res]
    ~(do
      (def ,buf (buffer/new 1024))
      (with-dyns [:out ,buf]
        (def ,res ,form)
        [,res (string ,buf)]))))

(do
  (def [result output]
    (capture-stdout
      (misc/timeit (loop [i :range [1 100000]] i) "foo:")))
  (assert (nil? result))
  (def m (peg/match '(* "foo: " (<- (some :S)) " seconds\n" -1) output))
  (assert (truthy? m) "timeit -- invalid output")
  (assert (scan-number (in m 0)) "timeit -- invalid number of seconds"))

(do
  (def [result output]
    (capture-stdout
      (misc/timeit "someresult")))
  (assert (= result "someresult") "timeit2 -- invalid return")
  (def m (peg/match '(* "Elapsed time: " (<- (some :S)) " seconds\n" -1) output))
  (assert (truthy? m) "timeit2 -- invalid output")
  (assert (scan-number (in m 0)) "timeit2 -- invalid number of seconds"))

#misc/set*
(do
  (var x 2)
  (var y 3)
  (misc/set* [x y] [y (+ x y)])
  (print x " " y)
  (assert (and (= x 3) (= y 5)) "set* 1"))

(do
  (def x @[2 3])
  (misc/set* [[x 0] [x 1]] [(in x 1) (+ (in x 0) (in x 1))])
  (assert (deep= x @[3 5])))

(end-suite)
