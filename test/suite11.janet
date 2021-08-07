(use ../spork/test)
(import ../spork/ev-utils :as eu)
(import ../spork/streams :as streams)

(start-suite 11)

(defn- stream-assert!
  [s]
  (assert (fiber? s))
  (assert (= :new (fiber/status s))))

(def s (streams/from-iterable [1 2 3]))
(stream-assert! s)
(assert (deep= @[1 2 3] (values s)))

(def s (streams/from-iterable @[1 2 3]))
(stream-assert! s)
(assert (deep= @[1 2 3] (values s)))

(def s (streams/from-iterable @[1 2 3]))
(def s2 (streams/from-iterable s))
(stream-assert! s)
(assert (deep= @[1 2 3] (values s2)))

(def s (streams/range 1 10))
(stream-assert! s)
(assert (deep= @[1 2 3 4 5 6 7 8 9] (values s)))

(def s (streams/concat [1] @[2] (streams/from-iterable [3 4 5])))
(stream-assert! s)
(assert (deep= @[1 2 3 4 5] (values s)))

(def s (streams/map inc [1 2 3]))
(stream-assert! s)
(assert (deep= @[2 3 4] (values s)))

(def s (streams/filter odd? [1 2 3]))
(stream-assert! s)
(assert (deep= @[1 3] (values s)))

(def s (streams/take 2 [1 2 3]))
(stream-assert! s)
(assert (deep= @[1 2] (values s)))

(def s (streams/take-while odd? [1 2 3]))
(stream-assert! s)
(assert (deep= @[1] (values s)))

(def s (streams/take-until even? [1 2 3]))
(stream-assert! s)
(assert (deep= @[1] (values s)))

(def s (streams/drop 1 [1 2 3]))
(stream-assert! s)
(assert (deep= @[2 3] (values s)))

(def s (streams/drop-while odd? [1 2 3]))
(stream-assert! s)
(assert (deep= @[2 3] (values s)))

(def s (streams/drop-until even? [1 2 3]))
(stream-assert! s)
(assert (deep= @[2 3] (values s)))

(def s (streams/cycle [1 2 3]))
(stream-assert! s)
(def taken (streams/take 10 s))
(assert (deep= @[1 2 3 1 2 3 1 2 3 1] (values taken)))

(end-suite)
