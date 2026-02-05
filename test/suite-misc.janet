(use ../spork/test)
(import ../spork/misc)

(start-suite)

#misc/dedent
(assert (= (misc/dedent "  a\n    b\n   c\n     d") "a\n  b\n c\n   d") "dedent")

#misc/set*
(do
  (var x 2)
  (var y 3)
  (misc/set* [x y] [y (+ x y)])
  (assert (and (= x 3) (= y 5)) "set* 1"))

(do
  (def x @[2 3])
  (misc/set* [[x 0] [x 1]] [(in x 1) (+ (in x 0) (in x 1))])
  (assert (deep= x @[3 5])))

#misc/trim-prefix

(assert (= (misc/trim-prefix "someprefix" "someprefixsomeprefixandmore") "someprefixandmore"))
(assert (= (misc/trim-prefix "🗲" "🗲🗲this-is-a-unicode-test🗲") "🗲this-is-a-unicode-test🗲"))


#misc/trim-suffix

(assert (= (misc/trim-suffix "somesuffix" "someprefix-midpart-somesuffixsomesuffix") "someprefix-midpart-somesuffix"))

(assert (= (misc/trim-suffix "🗲" "🗲this-is-a-unicode-test🗲🗲") "🗲this-is-a-unicode-test🗲"))

#misc/print-table
(def output
  (misc/capout
    (misc/print-table [{"aaaa" 1 "b" 2} {"aaaa" 4 "b" 5}] ["aaaa" "b" "b" "b" "b"])))
(def expected
  ```
╭────┬─┬─┬─┬─╮
│aaaa│b│b│b│b│
╞════╪═╪═╪═╪═╡
│   1│2│2│2│2│
│   4│5│5│5│5│
╰────┴─┴─┴─┴─╯
  ```)
(assert (= (-> output string/trim) (->> expected (string/replace-all "\r" "") string/trim)))

(def output2
  (misc/capout
    (misc/print-table [{"title" "你好世界" "value" 3} {"title" "小苹果" "value" 4}])))
(def expected2
  ```
╭────────┬─────╮
│  title │value│
╞════════╪═════╡
│你好世界│    3│
│  小苹果│    4│
╰────────┴─────╯
  ```)
(assert (= (-> output2 string/trim) (->> expected2 (string/replace-all "\r" "") string/trim)))

(assert
  (deep= (misc/map-keys string {1 2 3 4}) @{"1" 2 "3" 4})
  "map-keys in dictionary")
(assert
  (deep= (misc/map-keys string {1 2 3 {:a :b :c {:d 3}}})
         @{"1" 2 "3" @{"a" :b "c" @{"d" 3}}})
  "map-keys in nested struct")

(assert
  (deep= (misc/map-keys-flat string {1 [1 2 3] 3 [1 2]}) @{"1" [1 2 3] "3" [1 2]})
  "map-keys-flat in dictionary")
(assert
  (deep= (misc/map-keys-flat string {1 2 3 {:a :b :c {:d 3}}})
         @{"1" 2 "3" {:a :b :c {:d 3}}})
  "map-keys-flat in nested struct")

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
  (deep= (misc/select-keys {1 false 3 4 5 6} [1]) @{1 false})
  "selects key with false value")
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

(assert (= (misc/do-var res 0 (set res 100) (++ res))
           101)
        "do-var")

(assert (deep= (misc/do-def res @"" (buffer/push res "a")) @"a")
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

(assert (= (do (misc/vars a 2 b 1) a) 2)
        "vars")

(assert (= (do (misc/defs a 2 b 1) a) 2)
        "defs")

(let [always-true (misc/always true)]
  (assert (always-true) "always true")
  (assert (always-true) "always true")
  (assert (always-true) "always true")
  (assert (always-true 1 2 3) "always true (args)"))

(assert (= (misc/second [1 2 3]) 2)
        "second")
(assert (nil? (misc/second [1]))
        "second (short)")
(assert (nil? (misc/second []))
        "second (empty)")

(assert (= (misc/third [1 2 3]) 3)
        "third")
(assert (nil? (misc/third [1 2]))
        "third (short)")
(assert (nil? (misc/third []))
        "third (empty)")

(assert (= (misc/penultimate [1 2 3 4 5]) 4)
        "penultimate")
(assert (nil? (misc/penultimate [1]))
        "penultimate (short)")
(assert (nil? (misc/penultimate []))
        "penultimate (empty)")

(assert (= (misc/antepenultimate [1 2 3 4 5]) 3)
        "antepenultimate")
(assert (nil? (misc/antepenultimate [1 2]))
        "antepenultimate (short)")
(assert (nil? (misc/antepenultimate []))
        "antepenultimate (empty)")

(assert (= (misc/int/ 11 3) 3)
        "int/ (pos/pos)")
(assert (= 3 (misc/int/ -11 -3))
        "int/ (neg/neg)")
(assert (= -3 (misc/int/ -11 3))
        "int/ (neg/pos)")
(assert (= -3 (misc/int/ 11 -3))
        "int/ (pos/neg)")

(assert (= (misc/gett {:a {:b {:c :c}}} :a :b :c) :c)
        "gett")
(assert (nil? (misc/gett {:a {:b {:c :c}}} :d :b :c))
        "gett (nil)")

(assert
  (= (misc/do-var res 0 (misc/until (> res 3) (++ res)))
     4)
  "until")

(assert (deep= (misc/table-filter |(even? $1) @{:zero 0 :one 1 :two 2 :three 3})
               @{:zero 0 :two 2})
        "table-filter 1")
(assert (deep= (misc/table-filter |(odd? $1) @{:zero 0 :one 1 :two 2 :three 3})
               @{:one 1 :three 3})
        "table-filter 2")

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

(assert (deep= (misc/insert-sorted @[1 2 3 5] < 4)
               @[1 2 3 4 5])
        "insert-sorted 1")
(assert (deep= (misc/insert-sorted @[4 5] < 3 2 1)
               @[1 2 3 4 5])
        "array/insert-sorted 2")
(assert (deep= (misc/insert-sorted @[1 2 3] |(error "invoked callback needlessly!"))
               @[1 2 3])
        "array/insert-sorted 3")

(assert (deep= (misc/insert-sorted-by @[1 2 3 5] identity 4)
               @[1 2 3 4 5])
        "array/insert-sorted-by 1")
(assert (deep= (misc/insert-sorted-by @[4 5] identity 3 2 1)
               @[1 2 3 4 5])
        "array/insert-sorted-by 2")
(assert (deep= (misc/insert-sorted-by @[2] - 1 3)
               @[3 2 1])
        "array/insert-sorted-by 3")
(assert (deep= (misc/insert-sorted-by @[1 2 3] |(error "invoked callback needlessly!"))
               @[1 2 3])
        "array/insert-sorted-by 4")

(assert (deep= (misc/merge-sorted @[2 3 6 7] @[1 4 5 8])
               @[1 2 3 4 5 6 7 8])
        "misc/merge-sorted 1")
(assert (deep= (misc/merge-sorted @[3 2 1] @[8 7 6 5 4] >)
               @[8 7 6 5 4 3 2 1])
        "misc/merge-sorted 2")
(assert (deep= (misc/merge-sorted @[1 2 3 4 5 6 7 8] @[] |(error "invoked callback needlessly!"))
               @[1 2 3 4 5 6 7 8])
        "misc/merge-sorted 3")

(assert (deep= (misc/merge-sorted-by @[2 3 6 7] @[1 4 5 8] identity)
               @[1 2 3 4 5 6 7 8])
        "misc/merge-sorted-by 1")
(assert (deep= (misc/merge-sorted-by @[3 2 1] @[8 7 6 5 4] -)
               @[8 7 6 5 4 3 2 1])
        "misc/merge-sorted-by 2")
(assert (deep= (misc/merge-sorted-by @[1 2 3 4 5 6 7 8] @[] |(error "invoked callback needlessly!"))
               @[1 2 3 4 5 6 7 8])
        "misc/merge-sorted-by 3")

(end-suite)
