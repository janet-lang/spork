(use ../spork/test)
(use ../spork/infix)

(start-suite)

# Basic tests
(assert (deep= '(+ 1 2) (macex1 '($$ 1 + 2))))
(assert (deep= ~(,math/pow 1 2) (macex1 '($$ 1 ** 2))))
(assert (= ($$ 1 - 2 - 3 - 4) (- 1 2 3 4)))
(assert (= ($$ 1 + 2 + 3 + 4) (+ 1 2 3 4)))
(assert (= ($$ 1 * 2 * 3 * 4) (* 1 2 3 4)))
(assert (= ($$ 1 / 2 / 3 / 4) (/ 1 2 3 4)))
(assert (= ($$ 1 % 2 % 3 % 4) (% 1 2 3 4)))
(assert (= ($$ 2 ** 3 ** 4 + 1) (+ 1 (math/pow 2 (math/pow 3 4)))))

# Examples
(def a 123123)
(def b 12391)
(def y [10 20 30 40])
(def z :thing)
(defn good? [z] (not z))
(assert (= ($$ a + b ** 2) (+ a (math/pow b 2))))
(assert (= ($$ (a + b) ** 2) (math/pow (+ a b) 2)))
(assert (= ($$ y[2] + y[3]) (+ (in y 2) (in y 3))))
(assert (= ($$ a > b and ,(good? z)) (and (> a b) (good? z))))

# Logic (and or)
(assert (= ($$ true and nil) nil))
(assert (= ($$ true and not nil) true))
(assert (= ($$ false or not false) true))
(assert (= ($$ false or true and not false) true))
(assert (= ($$ false or true and ! false) true))

# Bit operations
(assert (= ($$ 1 << 1) 2))
(assert (= ($$ 1 >> 1) 0))
(assert (= ($$ 0xFF00 & 0xFF) 0))
(assert (= ($$ 0xFF00 band 0xFF) 0))
(assert (= ($$ 0xFF00 bor 0xFF) 0xFFFF))
(assert (= ($$ 0xFF00 ^ 0xFF) 0xFFFF))
(assert (= ($$ 0xFF0 ^ 0x0FF) 0xF0F))
(assert (= ($$ 0xFF00 bor 0xFF bor 0x10000) 0x1FFFF))

# Array indexing
(def an-array [:a :b :c 1 2 3])
(assert (= :b ($$ an-array[1])))
(assert-error "out of bounds" ($$ an-array[100]))

# Mutation with ++ and --
(var a 0)
(assert (= 11 ($$ ++ a + 10)))
(assert (= 10 ($$ -- a + 10)))

# Comparisons
(assert (= true ($$ 100 > 20)))
(assert (= false ($$ 10 > 20)))
(assert (= true ($$ 100 >= 20)))
(assert (= true ($$ 20 >= 20)))
(assert (= false ($$ 10 >= 20)))
(assert (= true ($$ 0 < 20)))
(assert (= false ($$ 20 < 20)))
(assert (= false ($$ 40 < 20)))
(assert (= true ($$ 0 <= 20)))
(assert (= true ($$ 20 <= 20)))
(assert (= false ($$ 40 <= 20)))
(assert (= true ($$ :a = :a)))
(assert (= false ($$ :b = :a)))
(assert (= false ($$ :a != :a)))
(assert (= true ($$ :b != :a)))
(assert (= false ($$ :a not= :a)))
(assert (= true ($$ :b not= :a)))
(assert ($$ 10 <= 20 and 30 < 40))

(end-suite)
