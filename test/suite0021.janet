(use ../spork/test)
(import ../spork/data :as d)

(start-suite 21)

(assert-docs "/spork/data")

(defn diff-assert [a b should-be msg]
  (assert (deep= (d/diff a b) should-be) msg))

(def cases
  [[1 1 @[nil nil 1] "Should be: Integers, same"]
   [1 2 @[1 2 nil] "Should be: Integers, different"]

   ["1" "1" @[nil nil "1"] "Should be: Strings, same"]
   ["1" "2" @["1" "2" nil] "Should be: Strings, different"]

   ["String" 1 @["String" 1 nil] "Should be: String and Integer, different"]

   [[1 2 3] [1 2 3] @[nil nil @[1 2 3]] "Should be: Tuples, same"]
   [[1 2 3] [1 2 3 4] @[nil @[nil nil nil 4] @[1 2 3]] "Should be: Tuples, element added"]
   [[1 2 3] [1 2] @[@[nil nil 3] nil @[1 2]] "Should be: Tuples, element removed"]
   [[1 2 3] [1 5 3] @[@[nil 2] @[nil 5] @[1 nil 3]] "Should be: Tuples, element changed"]
   [[1 2 3] [1 5] @[@[nil 2 3] @[nil 5] @[1]] "Should be: Tuples, element changed and element removed"]

   [@[1 2 3] @[1 2 3] @[nil nil @[1 2 3]] "Should be: Arrays, same"]
   [@[1 2 3] @[1 2 3 4] @[nil @[nil nil nil 4] @[1 2 3]] "Should be: Arrays, element added"]
   [@[1 2 3] @[1 2] @[@[nil nil 3] nil @[1 2]] "Should be: Arrays, element removed"]
   [@[1 2 3] @[1 5 3] @[@[nil 2] @[nil 5] @[1 nil 3]] "Should be: Arrays, element changed"]
   [@[1 2 3] @[1 5] @[@[nil 2 3] @[nil 5] @[1]] "Should be: Arrays, element changed and element removed"]

   [{:a 1 :b 2} {:a 1 :b 2} @[nil nil @{:a 1 :b 2}] "Should be: Structs, same"]
   [{:a 1 :b 2} {:a 1 :b 2 :c 3} @[nil @{:c 3} @{:a 1 :b 2}] "Should be: Structs, element added"]
   [{:a 1 :b 2} {:a 1} @[@{:b 2} nil @{:a 1}] "Should be: Structs, element removed"]
   [{:a 1 :b 2} {:a 1 :b 5} @[@{:b 2} @{:b 5} @{:a 1}] "Should be: Structs, element changed"]
   [{:a 1 :b 2} {:b 5} @[@{:a 1 :b 2} @{:b 5} nil] "Should be: Structs, element changed and element removed"]

   [@{:a 1 :b 2} @{:a 1 :b 2} @[nil nil @{:a 1 :b 2}] "Should be: Tables, same"]
   [@{:a 1 :b 2} @{:a 1 :b 2 :c 3} @[nil @{:c 3} @{:a 1 :b 2}] "Should be: Tables, element added"]
   [@{:a 1 :b 2} @{:a 1} @[@{:b 2} nil @{:a 1}] "Should be: Tables, element removed"]
   [@{:a 1 :b 2} @{:a 1 :b 5} @[@{:b 2} @{:b 5} @{:a 1}] "Should be: Tables, element changed"]
   [@{:a 1 :b 2} @{:b 5} @[@{:a 1 :b 2} @{:b 5} nil] "Should be: Tables, element changed and element removed"]

   [@{:a 1 :b {:c 1 :d 2}} @{:a 1 :b {:c 1 :d 2}} @[nil nil @{:a 1 :b @{:c 1 :d 2}}] "Should be: Nested Tables, same"]
   [@{:a 1 :b {:c 1 :d 2}} @{:a 1 :b {:c 1 :d 2 :e {:f 1 :g 2}} :h 3} @[nil @{:b @{:e {:f 1 :g 2}} :h 3} @{:a 1 :b @{:c 1 :d 2}}] "Should be: Nested Tables, element added"]
   [@{:a 1 :b {:c 1 :d 2}} @{:a 1 :b {:c 1}} @[@{:b @{:d 2}} nil @{:a 1 :b @{:c 1}}] "Should be: Nested Tables, element removed"]
   [@{:a 1 :b {:c 1 :d 2}} @{:a 1 :b {:c 1 :d 5}} @[@{:b @{:d 2}} @{:b @{:d 5}} @{:a 1 :b @{:c 1}}] "Should be: Nested Tables, element changed"]
   [@{:a 1 :b {:c 1 :d 2}} @{:b {:c 1 :d 5}} @[@{:a 1 :b @{:d 2}} @{:b @{:d 5}} @{:b @{:c 1}}] "Should be: Nested Tables, element changed and element removed"]

   [{:a 1 :b 2} @{:a 1 :b 2} @[nil nil @{:a 1 :b 2}] "Should be: Struct and Table, same"]
   [{:a 1 :b 2} @{:a 1 :b 2 :c 4 :d 5} @[nil @{:c 4 :d 5} @{:a 1 :b 2}] "Should be: Struct and Table, different"]

   [@[1 2 3] [1 2 3] @[nil nil @[1 2 3]] "Should be: Array and Tuple, same"]
   [@[1 2 3] [1 2 3 4 5] @[nil @[nil nil nil 4 5] @[1 2 3]] "Should be: Array and Tuple, different"]

   [@{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}}
    @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}}
    @[nil nil @{6 @{7 @{:f "test" :g @{8 @[9 10 11] :h 12}}} :a @[1 2 @{:b @{:c 3}}] 5 @[:d :e 4]}]
    "Should be: Nested Complex Data Structures, same"]

   [@{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}}
    @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 {:z 100} 11] :h 12}}}}
    @[@{6 @{7 @{:g @{8 @[nil nil 11]}}}} @{6 @{7 @{:g @{8 @[nil nil {:z 100} 11]}}}} @{5 @[:d :e 4] 6 @{7 @{:f "test" :g @{8 @[9 10] :h 12}}} :a @[1 2 @{:b @{:c 3}}]}]
    "Should be: Nested Complex Data Structures, deep insertion"]

   [@{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}}
    @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11]}}}}
    @[@{6 @{7 @{:g @{:h 12}}}} nil @{6 @{7 @{:g @{8 @[9 10 11]} :f "test"}} 5 @[:d :e 4] :a @[1 2 @{:b @{:c 3}}]}]
    "Should be: Nested Complex Data Structures, deep delete"]

   [@{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}}
    @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [:z 10 11] :h 12}}}}
    @[@{6 @{7 @{:g @{8 @[9]}}}} @{6 @{7 @{:g @{8 @[:z]}}}} @{6 @{7 @{:f "test" :g @{8 @[nil 10 11] :h 12}}} 5 @[:d :e 4] :a @[1 2 @{:b @{:c 3}}]}]
    "Should be: Nested Complex Data Structures, deep update"]

   [@{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}}
    @{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 {:z 100 :x 10000 :y 100000} :h 12}}}}
    @[@{6 @{7 @{:g @{8 [9 10 11]}}}} @{6 @{7 @{:g @{8 {:x 10000 :y 100000 :z 100}}}}} @{:a @[1 2 @{:b @{:c 3}}] 5 @[:d :e 4] 6 @{7 @{:f "test" :g @{:h 12}}}}]
    "Should be: Nested Complex Data Structures, deep update of a whole structure"]

   [@{:a [1 2 {:b {:c 3}}] 5 @[:d :e 4] 6 @{7 {:f "test" :g {8 [9 10 11] :h 12}}}}
    @{:zz [1 10000 {:b {:c 3}}] 5 @[:d :e 1000] 6 @{7 {:f "test" :g {8 [9 10 11 {:z 10}] :h 12}}}}
    @[@{5 @[nil nil 4] :a [1 2 {:b {:c 3}}]} @{5 @[nil nil 1000] 6 @{7 @{:g @{8 @[nil nil nil {:z 10}]}}} :zz [1 10000 {:b {:c 3}}]} @{5 @[:d :e] 6 @{7 @{:f "test" :g @{8 @[9 10 11] :h 12}}}}]
    "Should be: Nested Complex Data Structures, multiple deep updates"]])

(map |(diff-assert ;$) cases)

(import ../spork/base64)

#base64/encode
(assert (= (base64/encode "this is a test") "dGhpcyBpcyBhIHRlc3Q="))
(assert (= (base64/encode "") ""))
(assert (= (base64/encode "f") "Zg=="))
(assert (= (base64/encode "fo") "Zm8="))
(assert (= (base64/encode "foo") "Zm9v"))
(assert (= (base64/encode "foob") "Zm9vYg=="))
(assert (= (base64/encode "fooba") "Zm9vYmE="))
(assert (= (base64/encode "foobar") "Zm9vYmFy"))

#base64/decode
(assert (= (base64/decode "dGhpcyBpcyBhIHRlc3Q=") "this is a test"))
(assert (= (base64/decode "") ""))
(do (def some-string "\x1Cdawdawdadwdaw\xB0")
    (assert (= (base64/decode (base64/encode some-string)) some-string)))

(end-suite)
