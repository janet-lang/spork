(use ../spork/test)
(import spork/cc)

(start-suite)

(assert-no-error "no error 1" (cc/compile-c "a.o" "a.c"))
(assert-no-error "no error 2" (cc/compile-c++ "a.o" "a.cc"))
(assert-no-error "no error 3" (cc/link-shared-c ["a.o" "b.o"] "a.so"))
(assert-no-error "no error 4" (cc/link-shared-c++ ["a.o" "b.o"] "a.so"))
(assert-no-error "no error 5" (cc/link-executable-c ["a.o" "b.o"] "a"))
(assert-no-error "no error 6" (cc/link-executable-c++ ["a.o" "b.o"] "a"))
(assert-no-error "no error 7" (cc/make-archive ["a.o" "b.o"] "a.a"))

(assert-no-error "no error 8" (cc/compile-and-link-shared "a.so" "a.c" "b.c"))
(assert-no-error "no error 9" (cc/compile-and-link-shared "a.so" "a.cpp" "b.c"))
(assert-no-error "no error 10" (cc/compile-and-link-executable "a" "a.c" "b.c"))
(assert-no-error "no error 11" (cc/compile-and-link-executable "a" "a.cpp" "b.c"))
(assert-no-error "no error 12" (cc/compile-and-make-archive "a.a" "a.cpp" "b.c"))

(end-suite)
