(use test/helper)
(import spork/fmt)

(start-suite 0)
# only testing format-print as other fns are dependent on it
(def b @"")
(with-dyns [:out b]
  (fmt/format-print "(\n print\n \"HOHOHO\")"))
(assert (deep= b @"(print\n  \"HOHOHO\")\n") "format-print")
(end-suite)
