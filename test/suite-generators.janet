(use ../spork/test)
(import ../spork/ev-utils :as eu)
(import ../spork/generators :as generators)

(start-suite)

(defn- generator-assert!
  [s]
  (assert (fiber? s))
  (assert (= :new (fiber/status s))))

# from-iterable
(def s (generators/from-iterable [1 2 3]))
(generator-assert! s)
(assert (deep= @[1 2 3] (generators/to-array s)))

(def s (generators/from-iterable @[1 2 3]))
(generator-assert! s)
(assert (deep= @[1 2 3] (generators/to-array s)))

(def s (generators/from-iterable @[1 2 3]))
(def s2 (generators/from-iterable s))
(generator-assert! s)
(assert (deep= @[1 2 3] (generators/to-array s2)))

# range
(def s (generators/range 1 10))
(generator-assert! s)
(assert (deep= @[1 2 3 4 5 6 7 8 9] (generators/to-array s)))

(def s (generators/range 1 10 2))
(generator-assert! s)
(assert (deep= @[1 3 5 7 9] (generators/to-array s)))

(def s (generators/range 1 3 2))
(generator-assert! s)
(assert (deep= @[1] (generators/to-array s)))

(def s (generators/range 5 1 -1))
(generator-assert! s)
(assert (deep= @[5 4 3 2] (generators/to-array s)))

(def s (generators/range 5 1 -2))
(generator-assert! s)
(assert (deep= @[5 3] (generators/to-array s)))

(def s (generators/range 5 3 -2))
(generator-assert! s)
(assert (deep= @[5] (generators/to-array s)))

# concat
(def s (generators/concat [1] @[2] (generators/from-iterable [3 4 5])))
(generator-assert! s)
(assert (deep= @[1 2 3 4 5] (generators/to-array s)))

# map
(def s (generators/map inc [1 2 3]))
(generator-assert! s)
(assert (deep= @[2 3 4] (generators/to-array s)))

(def s (generators/map tuple [1 2 3] [4 5 6]))
(generator-assert! s)
(assert (deep= @[[1 4] [2 5] [3 6]] (generators/to-array s)))

(def s (generators/map tuple [1 2 3] [4 5 6] @[7 8 9]))
(generator-assert! s)
(assert (deep= @[[1 4 7] [2 5 8] [3 6 9]] (generators/to-array s)))

(def s (generators/map tuple [1 2 3] [4 5 6] @[7 8 9] (generators/from-iterable [-1 -2 -3])))
(generator-assert! s)
(assert (deep= @[[1 4 7 -1] [2 5 8 -2] [3 6 9 -3]] (generators/to-array s)))

(def s (generators/map tuple [1 2 3] [4 5 6] [7 8 9] [10 11 12] [-1 -2 -3]))
(generator-assert! s)
(assert (deep= @[[1 4 7 10 -1] [2 5 8 11 -2] [3 6 9 12 -3]] (generators/to-array s)))

(def s (generators/map tuple [1 2 3] [4 5 6] [7 8 9] [10 11 12] [-1 -2]))
(generator-assert! s)
(assert (deep= @[[1 4 7 10 -1] [2 5 8 11 -2]] (generators/to-array s)))

# mapcat
(def s (generators/mapcat (fn [x] [x x]) [1 2 3]))
(generator-assert! s)
(assert (deep= @[1 1 2 2 3 3] (generators/to-array s)))

(def s (generators/mapcat tuple [1 2 3] [4 5 6]))
(generator-assert! s)
(assert (deep= @[1 4 2 5 3 6] (generators/to-array s)))

(def s (generators/mapcat tuple [1 2 3] [4 5 6] @[7 8 9]))
(generator-assert! s)
(assert (deep= @[1 4 7 2 5 8 3 6 9] (generators/to-array s)))

(def s (generators/mapcat tuple [1 2 3] [4 5 6] @[7 8 9] (generators/from-iterable [10 11 12])))
(generator-assert! s)
(assert (deep= @[1 4 7 10 2 5 8 11 3 6 9 12] (generators/to-array s)))

(def s (generators/mapcat tuple [1 2 3] [4 5 6] [7 8 9] [10 11 12] [-1 -2 -3]))
(generator-assert! s)
(assert (deep= @[1 4 7 10 -1 2 5 8 11 -2 3 6 9 12 -3] (generators/to-array s)))

(def s (generators/mapcat tuple [1 2 3] [4 5 6] [7 8 9] [10 11 12] [-1 -2]))
(generator-assert! s)
(assert (deep= @[1 4 7 10 -1 2 5 8 11 -2] (generators/to-array s)))

# filter
(def s (generators/filter odd? [1 2 3]))
(generator-assert! s)
(assert (deep= @[1 3] (generators/to-array s)))

# take
(def s (generators/take 2 [1 2 3]))
(generator-assert! s)
(assert (deep= @[1 2] (generators/to-array s)))

# take-while
(def s (generators/take-while odd? [1 2 3]))
(generator-assert! s)
(assert (deep= @[1] (generators/to-array s)))

# take-until
(def s (generators/take-until even? [1 2 3]))
(generator-assert! s)
(assert (deep= @[1] (generators/to-array s)))

# drop
(def s (generators/drop 1 [1 2 3]))
(generator-assert! s)
(assert (deep= @[2 3] (generators/to-array s)))

# drop-while
(def s (generators/drop-while odd? [1 2 3]))
(generator-assert! s)
(assert (deep= @[2 3] (generators/to-array s)))

# drop-until
(def s (generators/drop-until even? [1 2 3]))
(generator-assert! s)
(assert (deep= @[2 3] (generators/to-array s)))

# cycle
(def s (generators/cycle [1 2 3]))
(generator-assert! s)
(def taken (generators/take 10 s))
(assert (deep= @[1 2 3 1 2 3 1 2 3 1] (generators/to-array taken)))

(def s (generators/cycle {:a 1 :b 2 :c 3}))
(def taken (generators/take 10 s))
(def taken-array (generators/to-array taken))
(assert  (= 10 (length taken-array)))
(assert (deep= @[1 2 3] (sorted (distinct taken-array))))

# interleave
(def s (generators/interleave [1 2 3]))
(generator-assert! s)
(assert (deep= @[1 2 3] (generators/to-array s)))

(def s (generators/interleave [1 2 3] [4 5 6]))
(generator-assert! s)
(assert (deep= @[1 4 2 5 3 6] (generators/to-array s)))

(def s (generators/interleave [1 2 3] [4 5 6] @[7 8 9]))
(generator-assert! s)
(assert (deep= @[1 4 7 2 5 8 3 6 9] (generators/to-array s)))

(def s (generators/interleave [1 2 3] [4 5 6] @[7 8 9] (generators/from-iterable @[10 11 12])))
(generator-assert! s)
(assert (deep= @[1 4 7 10 2 5 8 11 3 6 9 12] (generators/to-array s)))

(def s (generators/interleave [1 2 3] [4 5 6] [7 8 9] [10 11 12] [-1 -2 -3]))
(generator-assert! s)
(assert (deep= @[1 4 7 10 -1 2 5 8 11 -2 3 6 9 12 -3] (generators/to-array s)))

(def s (generators/interleave [1 2 3] [4 5 6] [7 8 9] [10 11] [-1 -2 -3]))
(generator-assert! s)
(assert (deep= @[1 4 7 10 -1 2 5 8 11 -2] (generators/to-array s)))

(end-suite)
