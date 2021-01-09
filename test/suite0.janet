(use ./helper)
(import ../spork/fmt)

(start-suite 0)

# only testing format-print as other fns are dependent on it
(def b @"")
(with-dyns [:out b]
  (fmt/format-print "(\n print\n \"HOHOHO\")"))
(assert (deep= b @"(print\n  \"HOHOHO\")\n") "format-print")

 # regresion with comment in the collection literals
(buffer/clear b)
(with-dyns [:out b]
  (fmt/format-print "{:a 0\n:b 1 # test comment\n}"))
(assert (deep= b @"{:a 0\n :b 1 # test comment\n}\n") "format-print comment in collection 1")

(buffer/clear b)
(with-dyns [:out b]
  (fmt/format-print "[:a       0\n:b\n# test comment\n]"))
(assert (deep= b @"[:a 0\n :b\n # test comment\n]\n") "format-print comment in collection 2")

(buffer/clear b)
(with-dyns [:out b]
  (fmt/format-print "()"))
(assert (deep= b @"()\n") "format-print ()")

(end-suite)
