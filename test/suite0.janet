(use ../spork/test)
(import ../spork/fmt)

(start-suite 0)

# only testing format-print as other fns are dependent on it
(do
  (def res
    (capture-stdout
      (fmt/format-print "(\n print\n  \"HOHOHO\")")))
  (assert (= res [nil "(print\n  \"HOHOHO\")\n"]) "format-print"))

# regresion with comment in the collection literals
(do
  (def res
    (capture-stdout
      (fmt/format-print "{:a 0\n:b 1 # test comment\n}")))
  (assert (= res [nil "{:a 0\n :b 1 # test comment\n}\n"]) "format-print comment in collection 1"))

(do
  (def res
    (capture-stdout
      (fmt/format-print "[:a       0\n:b\n# test comment\n]")))
  (assert (= res [nil "[:a 0\n :b\n # test comment\n]\n"]) "format-print comment in collection 2"))

(do
  (def res
    (capture-stdout
      (fmt/format-print "()")))
  (assert (= res [nil "()\n"]) "format-print empty form"))

(do
  (def res
    (capture-stdout
      (fmt/format-print "( )")))
  (assert (= [nil "()\n"]) "format-print empty form with whitespace"))

(do
  (def res
    (capture-stdout
      (fmt/format-print "# a comment")))
  (assert (= [nil "# a comment\n\n"]) "format-print only comment"))

(do
  (def res
    (capture-stdout
      (try
        (fmt/format-print "print )")
        ([err]
         (print "error")))))
  (assert (= [nil "error\n"]) "format-print errors with unbalanced parenthesis"))

(end-suite)
