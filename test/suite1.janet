(use test/helper)
(import spork/misc)

(start-suite 1)
(assert (= (misc/dedent "  a\n    b\n   c\n     d") "a\n  b\n c\n   d") "dedent")
(end-suite)
