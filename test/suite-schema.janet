(use ../spork/test)
(import ../spork/schema :as schema)

(start-suite)

(def c1 (schema/predicate :number))
(assert (not (c1 :test)) "checker c1 1")
(assert (c1 0) "checker c1 2")
(assert (not (c1 :number)) "checker c1 3")
(assert (c1 math/inf) "checker c1 4")
(assert (c1 math/nan) "checker c1 5")

(def c2 (schema/predicate (or :number (and (or :array :tuple) (length 1 3) (values :number)))))
(assert (c2 2) "checker c2 1")
(assert (c2 -1) "checker c2 2")
(assert (not (c2 [])) "checker c2 3")
(assert (c2 [2 3]) "checker c2 4")
(assert (not (c2 [3 3 :nope])) "checker c2 5")
(assert (not (c2 [3 3 4 5])) "checker c2 6")

(def v1 (schema/validator :number))
(assert-no-error "validator v1 1" (v1 0))
(assert-no-error "validator v1 2" (v1 math/nan))
(assert-error "validator v1 3" (v1 :hello))
(assert-error "validator v1 4" (v1 nil))

(def v2
  (schema/validator
    (props
      :a :number
      :b :number
      :c (or :string nil))))
(assert-no-error "validator v2 1" (v2 {:a 1 :b 2}))
(assert-no-error "validator v2 2" (v2 {:a 1 :b 2 :c "hello"}))

(defn pos-string? [x] (if-let [y (scan-number x)] (pos? y)))
(def v3
  (schema/validator
    (or
      (and :number (pred pos?))
      (and :string (pred pos-string?)))))
(assert-no-error "validator v3 1" (v3 1))
(assert-error "validator v3 2" (v3 -1))
(assert-no-error "validator v3 3" (v3 "1"))
(assert-error "validator v3 4" (v3 "-1"))
(assert-error "validator v3 5" (v3 :-1))

# switch "and" and "or" in the v3 validator
(def v4
  (schema/validator
    (and
      (or :number (pred pos?))
      (or :string (pred pos-string?)))))
(assert-error "validator v4 1" (v4 1))
(assert-error "validator v4 2" (v4 -1))
(assert-no-error "validator v4 3" (v4 "1")) # strings are considered "pos?"
(assert-no-error "validator v4 4" (v4 "-1"))

(end-suite)
