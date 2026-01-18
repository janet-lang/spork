(use ../spork/test)
(use ../spork/math)
(import spork/tarray)

(start-suite)

(defn inspect-tarray
  [x]
  (def a @[])
  (for i 0 (tarray/length x) (array/push a (x i)))
  (pp a))

(assert-no-error
 "create some typed arrays"
 (do
   (def a (tarray/new :float64 10))
   (def b (tarray/new :float64 5 2 0 a))
   (def c (tarray/new :uint32 20))))

(assert-no-error
 "create some typed arrays from a buffer"
 (do
   (def buf (tarray/buffer (+ 64 (* (+ 1 (* (- 10 1) 2)) 8))))
   (def b (tarray/new :float64 10 2 64 buf))))

(def a (tarray/new :float64 10))
(def b (tarray/new :float64 5 2 0 a))

(assert-no-error
 "fill tarray"
 (for i 0 (tarray/length a)
      (set (a i) i)))

(assert (= (tarray/buffer a) (tarray/buffer b)) "tarray views pointing same buffer")
(assert (= (a 2) (b 1) ) "tarray views pointing same buffer")
(assert (= ((tarray/slice b) 3) (b 3) (a 6) 6) "tarray slice")
(assert (= ((tarray/slice b 1) 2) (b 3) (a 6) 6) "tarray slice")
(assert (= (:length a) (length a)) "length method and function")

(assert (= ((unmarshal (marshal b)) 3) (b 3)) "marshal")

# Janet issue 408
(assert-error :invalid-type (tarray/new :int32 10 1 0 (int/u64 7)) "tarray/new should only allow tarray or buffer for last argument")
(def ta (tarray/new :int32 10))
(assert (= (next a nil) 0) "tarray next 1")
(assert (= (next a 0) 1) "tarray next 2")
(assert (= (next a 8) 9) "tarray next 3")
(assert (nil? (next a 9)) "tarray next 4")
(put ta 3 7)
(put ta 9 7)
(assert (= 2 (count |(= $ 7) ta)) "tarray count")

# int64 typed arrays
(def i64 int/s64)
(def u64 int/u64)
(assert (let [t (tarray/new :int64 10)
              b (i64 1000)]
          (set (t 0) 1000)
          (set (t 1) b)
          (set (t 2) "1000")
          (set (t 3) (t 0))
          (set (t 4) (u64 1000))
          (and
           (= (t 0) (t 1))
           (= (t 1) (t 2))
           (= (t 2) (t 3))
           (= (t 3) (t 4))
           ))
        "int64 typed arrays")

# Janet Issue #142

(def buffer (tarray/buffer 8))
(def buffer-float64-view (tarray/new :float64 1 1 0 buffer))
(def buffer-uint32-view (tarray/new :uint32 2 1 0 buffer))

(set (buffer-uint32-view 1) 0xfffe9234)
(set (buffer-uint32-view 0) 0x56789abc)

(assert (buffer-float64-view 0) "issue #142 nanbox hijack 1")
(assert (= (type (buffer-float64-view 0)) :number) "issue #142 nanbox hijack 2")
(assert (= (type (unmarshal @"\xC8\xbc\x9axV4\x92\xfe\xff")) :number) "issue #142 nanbox hijack 3")


#construct random ta
(math/seedrandom 12345)
(def arr (tarray/new :float64 100))
(for i 0 (tarray/length arr)
  (put arr i (math/random)))

(math/seedrandom 123456)
(def array2 (tarray/new :float64 100))
(for i 0 (tarray/length array2)
  (put array2 i (math/random)))

(assert (approx-eq 0.208122 (median-absolute-deviation arr) 0.00001) "median-absolute-deviation")
(assert (approx-eq 0.274348 (sample-standard-deviation arr) 0.000001) "sample-standard-deviation")
(assert (approx-eq 0.272973 (standard-deviation arr) 0.000001) "standard-deviation")
(assert (let [[i a] (extent arr)]
           (and (approx-eq i 0.00746957 0.000001) (approx-eq a 0.973551 0.000001)))  "extent")
(assert (approx-eq 48.7728 (sum-compensated arr) 0.000001) "sum-compensated")
(assert (approx-eq 0.558921 (root-mean-square arr) 0.000001) "root-mean-square")
(assert (approx-eq -0.124152 (sample-skewness  arr) 0.00001) "sample-skewness ")
(assert (approx-eq 0.0745142 (variance arr) 0.000001) "variance")
(assert (approx-eq 0.0752669 (sample-variance arr) 0.000001) "sample-variance")
(assert (approx-eq 0.520372 (median arr) 0.000001) "median")
(assert (approx-eq 0.645951 (mode arr) 0.000001) "mode")
(assert (approx-eq 0.409312 (interquartile-range arr) 0.000001) "interquartile-range")
(assert (approx-eq 0.348654 (geometric-mean arr) 0.00001) "geometric-mean")
(assert (approx-eq 0.122595 (harmonic-mean arr) 0.00001) "harmonic-mean")


(assert (approx-eq 0.556132 (quantile-sorted arr 0.5) 0.000001) "quantile-sorted")
(assert (approx-eq 0.520372 (quantile arr 0.5) 0.000001) "quantile")
(assert (approx-eq 0.63 (quantile-rank-sorted arr 0.5) 0.000001) "quantile-rank-sorted")
(assert (approx-eq 0.49 (quantile-rank arr 0.5) 0.000001) "quantile-rank")
(assert (approx-eq 7.45142 (sum-nth-power-deviations arr 2) 0.000001) "sum-nth-power-deviations")
(assert-no-error (sample-covariance arr array2) "sample-covariance")
(assert-no-error (sample-correlation arr array2) "sample-correlation")
(assert-no-error (t-test arr 3) "t-test array")
(assert-no-error (t-test-2 arr array2) "t-test-2")

# Do shuffle test last!
(assert-no-error (shuffle-in-place arr) "shuffle-in-place")

(assert-no-error (permutation-test arr array2) "permutation-test array")

(end-suite)

