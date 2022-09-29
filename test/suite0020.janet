(use ../spork/test)
(import ../spork/path)
(import ../spork/misc)

(start-suite 20)

(assert (= (path/posix/relpath "dir1" "dir2") "../dir2"))

(assert (= (path/win32/relpath "dir1" "dir2") "..\\dir2"))

(assert (= (path/relpath "dir1" "dir2") (path/join ".." "dir2")))

(assert (= (path/posix/relpath "a/bit/deeper/with/some/nested/dir" "a/bit/deeper/with/other/nested/dir") "../../../other/nested/dir"))

(assert (= (path/posix/relpath "a/nested/directory/with/a/few/children" "a/nested/directory/with/different/children") "../../../different/children"))

(assert-docs "/spork/test")
(assert-docs "/spork/argparse")
(assert-docs "/spork/fmt")
(assert-docs "/spork/generators")
(assert-docs "/spork/getline")
(assert-docs "/spork/htmlgen")
(assert-docs "/spork/http")
(assert-docs "/spork/httpf")
(assert-docs "/spork/misc")
(assert-docs "/spork/msg")
(assert-docs "/spork/netrepl")
(assert-docs "/spork/path")
(assert-docs "/spork/regex")
(assert-docs "/spork/rpc")
(assert-docs "/spork/schema")
(assert-docs "/spork/sh")
(assert-docs "/spork/tasker")
(assert-docs "/spork/temple")

(assert-docs "spork/json")
(assert-docs "spork/tarray")
(assert-docs "spork/rawterm")
(assert-docs "spork/utf8")

(assert
  (deep= (misc/map-keys string {1 2 3 4}) @{"1" 2 "3" 4})
  "map-keys in dictionary")
(assert
  (deep= (misc/map-keys string {1 2 3 {:a :b :c {:d 3}}})
         @{"1" 2 "3" @{"a" :b "c" @{"d" 3}}})
  "map-keys in nested struct")

(assert
  (deep= (misc/map-vals string {1 2 3 4}) @{1 "2" 3 "4"})
  "map-vals of the dictionary 1")
(assert
  (deep= (misc/map-vals type {1 2 3 {:a :b}}) @{1 :number 3 :struct})
  "map-vals of the dictionary 1")

(assert
  (deep= (misc/select-keys {1 2 3 4 5 6} [1 5]) @{1 2 5 6})
  "selects keys from dictionary")
(assert
  (deep= (misc/select-keys {1 2 3 4 5 6} [1]) @{1 2})
  "selects key from dictionary")
(assert
  (deep= (misc/select-keys {1 2 3 4 5 {:a :b}} [1 5]) @{1 2 5 {:a :b}})
  "selects keys from nested dictionary")

(assert
  (deep=
    (misc/cond-> @{:a :b}
                 (pos? 1) (put :a :c)
                 (pos? 0) (put :a :d))
    @{:a :c})
  "cond->")

(assert
  (deep=
    (misc/cond->> @{:a :b}
                  (pos? 1) (merge {:d :e})
                  (pos? 0) (merge {:e :f}))
    @{:a :b
      :d :e})
  "cond->>")

(assert
  (=
    (do
      (def Proto @{:greet |(string "Hello " ($ :name))})
      (def t (misc/make Proto :name "pepe"))
      (:greet t))
    "Hello pepe")
  "make")

(assert (one? (misc/do-var res 0 (++ res)))
        "do-var")

(assert (deep= @"a" (misc/do-def res @"" (buffer/push res "a")))
        "do-def")

(assert (deep= (misc/capout (prin "HOHOHO"))
               @"HOHOHO")
        "capout one")
(assert (deep= (misc/capout
                 (def p "HOHOHO")
                 (prin p))
               @"HOHOHO")
        "capout many")

(assert (deep= (misc/caperr (eprin "HOHOHO"))
               @"HOHOHO")
        "caperr one")
(assert (deep= (misc/caperr
                 (def p "HOHOHO")
                 (eprin p))
               @"HOHOHO")
        "caperr many")

(assert (= 2 (do (misc/vars a 2 b 1) a))
        "vars")

(assert (= 2 (do (misc/defs a 2 b 1) a))
        "defs")

(let [always-true (misc/always true)]
  (assert (always-true) "always true")
  (assert (always-true) "always true")
  (assert (always-true) "always true")
  (assert (always-true 1 2 3) "always true (args)"))

(assert (= 2 (misc/second [1 2 3]))
        "second")
(assert (nil? (misc/second [1]))
        "second (short)")
(assert (nil? (misc/second []))
        "second (empty)")

(assert (= 3 (misc/third [1 2 3]))
        "third")
(assert (nil? (misc/third [1 2]))
        "third (short)")
(assert (nil? (misc/third []))
        "third (empty)")

(assert (= 4 (misc/penultimate [1 2 3 4 5]))
        "penultimate")
(assert (nil? (misc/penultimate [1]))
        "penultimate (short)")
(assert (nil? (misc/penultimate []))
        "penultimate (empty)")

(assert (= 3 (misc/antepenultimate [1 2 3 4 5]))
        "antepenultimate")
(assert (nil? (misc/antepenultimate [1 2]))
        "antepenultimate (short)")
(assert (nil? (misc/antepenultimate []))
        "antepenultimate (empty)")

(assert (= 3 (misc/int/ 11 3))
        "int/ (pos/pos)")
(assert (= 3 (misc/int/ -11 -3))
        "int/ (neg/neg)")
(assert (= -3 (misc/int/ -11 3))
        "int/ (neg/pos)")
(assert (= -3 (misc/int/ 11 -3))
        "int/ (pos/neg)")

(assert (= :c (misc/gett {:a {:b {:c :c}}} :a :b :c))
        "gett")
(assert (nil? (misc/gett {:a {:b {:c :c}}} :d :b :c))
        "gett (nil)")

(assert
  (= 4 (misc/do-var res 0
                    (misc/until (> res 3) (++ res))))
  "until")

(assert (deep= (misc/table/filter |(even? $1) @{:zero 0 :one 1 :two 2 :three 3})
               @{:zero 0 :two 2})
        "table/filter 1")
(assert (deep= (misc/table/filter |(odd? $1) @{:zero 0 :one 1 :two 2 :three 3})
               @{:one 1 :three 3})
        "table/filter 2")

(assert (deep= (misc/buffer/reverse @"abcd") @"dcba")
        "buffer/reverse (even length)")
(assert (deep= (misc/buffer/reverse @"abcde") @"edcba")
        "buffer/reverse (odd length)")

(assert (= (misc/string->int "101" 2) 2r101)
        "string->int (base 2)")
(assert (= (misc/string->int "42") 10r42)
        "string->int (base 10 default)")
(assert (= (misc/string->int "42" 16) 16r42)
        "string->int (base 16)")
(assert (= (misc/string->int "42z" 36) 36r42z)
        "string->int (base 36)")

(assert (= (misc/int->string 36r42z) (string 36r42z))
        "int->string (base 10 default)")
(assert (= (misc/int->string 2r11011011 2) "11011011")
        "int->string (base 2)")
(assert (= (misc/int->string 16rabacaba 16) "abacaba")
        "int->string (base 16)")
(assert (= (misc/int->string 36rxyzzy 36) "xyzzy")
        "int->string (base 36)")

(assert (= 0 (misc/first-index-where (misc/always true) [1 2 3]))
        "first-index-where 1")
(assert (nil? (misc/first-index-where (misc/always true) []))
        "first-index-where 2")
(assert (nil? (misc/first-index-where (misc/always false) [1 2 3]))
        "first-index-where 3")
(assert (= 2 (misc/first-index-where (partial <= 3) [1 2 3 4 5]))
        "first-index-where 4")

(assert (deep= (misc/array/insert-sorted @[1 2 3 5] < 4)
               @[1 2 3 4 5])
        "array/insert-sorted 1")
(assert (deep= (misc/array/insert-sorted @[4 5] < 3 2 1)
               @[1 2 3 4 5])
        "array/insert-sorted 2")
(assert (deep= (misc/array/insert-sorted @[1 2 3] |(error "invoked callback needlessly!"))
               @[1 2 3])
        "array/insert-sorted 3")

(assert (deep= (misc/array/insert-sorted-by @[1 2 3 5] identity 4)
               @[1 2 3 4 5])
        "array/insert-sorted-by 1")
(assert (deep= (misc/array/insert-sorted-by @[4 5] identity 3 2 1)
               @[1 2 3 4 5])
        "array/insert-sorted-by 2")
(assert (deep= (misc/array/insert-sorted-by @[2] - 1 3)
               @[3 2 1])
        "array/insert-sorted-by 3")
(assert (deep= (misc/array/insert-sorted-by @[1 2 3] |(error "invoked callback needlessly!"))
               @[1 2 3])
        "array/insert-sorted-by 4")

(assert misc/peg-grammar
        "peg grammar")
(assert (misc/setup-peg-grammar)
        "setup-peg-grammar")
(assert (deep= (peg/match ~(* :cap-to-crlf :crlf) "hoho\r\n\r\n")
               @["hoho"])
        "peg-grammar 1")
(assert (deep= (peg/match ~(* ':toe) "hoho")
               @["hoho"])
        "peg-grammar 2")

(assert (string? (misc/pgp/hex->word "01" 0)) "pgp/hex->word returns string")
(assert (= (misc/pgp/hex->word "01" 0) "absurd") "pgp/hex->word returns pgp/word")
(assert (= (misc/pgp/hex->word "0Y" 0) nil) "pgp/hex->word returns nil for wrong hex")
(assert (string? (misc/pgp/word->hex "absurd")) "pgp/hex->word returns string")
(assert (deep= (misc/pgp/hexs->words "01d1 02EE")
               @["absurd" "scavenger" "accrue" "universe"])
        "pgp/hex->words returns array of words")
(assert-error "pgp/hex->words errors out on wrong hex"
              (misc/pgp/hexs->words "01d1 02YE"))
(assert (nil? (misc/pgp/word->hex "absurdz"))
        "pgp/hex->word returns nil for unknown word")
(assert (= (misc/pgp/word->hex "absurd") "01") "pgp/hex->word returns string")
(assert (deep= (misc/pgp/words->hexs "absurd-scavenger accrue_universe upshot.village")
               @["01" "D1" "02" "EE" "F4" "F6"])
        "pgp/hex->words returns array of words")
(assert-error "pgp/hex->words errors out when there is unknown pgp/word"
              (misc/pgp/words->hexs "absurdz-scavenger accrue_universe"))

(end-suite)
