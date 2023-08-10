(use ../spork/test)
(use ../spork/math)
(import ../build/spork/tarray)

(start-suite 15)

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
   (def buf (buffer/new (+ 64 (* (+ 1 (* (- 10 1) 2)) 8))))
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

(def buffer (buffer/new 8))
(def buffer-float64-view (tarray/new :float64 1 1 0 buffer))
(def buffer-uint32-view (tarray/new :uint32 2 1 0 buffer))

(set (buffer-uint32-view 1) 0xfffe9234)
(set (buffer-uint32-view 0) 0x56789abc)

(assert (buffer-float64-view 0) "issue #142 nanbox hijack 1")
(assert (= (type (buffer-float64-view 0)) :number) "issue #142 nanbox hijack 2")
(assert (= (type (unmarshal @"\xC8\xbc\x9axV4\x92\xfe\xff")) :number) "issue #142 nanbox hijack 3")


#construct random ta
(math/seedrandom 12345)
(def array (tarray/new :float64 100))
(for i 0 (tarray/length array)
  (put array i (math/random)))

(math/seedrandom 123456)
(def array2 (tarray/new :float64 100))
(for i 0 (tarray/length array2)
  (put array2 i (math/random)))

(assert (approx-eq 0.208122 (median-absolute-deviation array) 0.00001) "median-absolute-deviation")
(assert (approx-eq 0.274348 (sample-standard-deviation array) 0.000001) "sample-standard-deviation")
(assert (approx-eq 0.272973 (standard-deviation array) 0.000001) "standard-deviation")
(assert (let [[i a] (extent array)]
           (and (approx-eq i 0.00746957 0.000001) (approx-eq a 0.973551 0.000001)))  "extent")
(assert (approx-eq 48.7728 (sum-compensated array) 0.000001) "sum-compensated")
(assert (approx-eq 0.558921 (root-mean-square array) 0.000001) "root-mean-square")
(assert (approx-eq -0.124152 (sample-skewness  array) 0.00001) "sample-skewness ")
(assert (approx-eq 0.0745142 (variance array) 0.000001) "variance")
(assert (approx-eq 0.0752669 (sample-variance array) 0.000001) "sample-variance")
(assert-no-error (shuffle-in-place array) "shuffle-in-place")
(assert (approx-eq 0.520372 (median array) 0.000001) "median")
(assert (approx-eq 0.645951 (mode array) 0.000001) "mode")
(assert (approx-eq 0.409312 (interquartile-range array) 0.000001) "interquartile-range")
(assert (approx-eq 0.348654 (geometric-mean array) 0.00001) "geometric-mean")
(assert (approx-eq 0.122595 (harmonic-mean array) 0.00001) "harmonic-mean")

(assert (approx-eq 0.556132 (quantile-sorted array 0.5) 0.000001) "quantile-sorted")
(assert (approx-eq 0.520372 (quantile array 0.5) 0.000001) "quantile")
(assert (approx-eq 0.63 (quantile-rank-sorted array 0.5) 0.000001) "quantile-rank-sorted")
(assert (approx-eq 0.49 (quantile-rank array 0.5) 0.000001) "quantile-rank")
(assert (approx-eq 7.45142 (sum-nth-power-deviations array 2) 0.000001) "sum-nth-power-deviations")
(assert-no-error (sample-covariance array array2) "sample-covariance")
(assert-no-error (sample-correlation array array2) "sample-correlation")
(assert-no-error (t-test array 3) "t-test array")
(assert-no-error (t-test-2 array array2) "t-test-2")
(assert-no-error (permutation-test array array2) "permutation-test array")


##some bug regarding iteration
(defn get-random-tarray [size seed]
  (math/seedrandom seed)
  (def array (tarray/new :float64 size))
  (for i 0 (tarray/length array)
    (put array i (math/random)))
  array)

(def randomtarrays @{
  :a (get-random-tarray 20000 123)
  :b (get-random-tarray 20000 123)
  :c (get-random-tarray 20000 123)
  :d (get-random-tarray 20000 123)
  })

# sometimes the crash is at another index but on my machine it's at around value #2777
# sometimes the crash happens right after that value, sometimes the script keeps executing across some sort of garbagedata
(let [count (tarray/length (randomtarrays :a))
      refbuf (get randomtarrays :a)]
  (pp randomtarrays)
  (each buf randomtarrays
    (pp (tarray/properties buf))
    (pp (get buf 0))
    (pp (get buf 1))
    (pp (get buf 2))
    (pp (get buf (- count 3)))
    (pp (get buf (- count 2)))
    (pp (get buf (- count 1))))
  #(print "Press any key to continue")
  #(getline)
  (for i 0 count
# it looks like the buffer is collected right after printing 2767/20000:
    (prin " now: " i "/" count ". ")
    (pp (length randomtarrays))
    (eachp [k buf] randomtarrays
# i.e. value is empty while refvalue contains a double, and k (line below this) is suddenly a floating point value instead of a b c or d.
      (prin "out " k " ")
      (let [_ (prin "let ") # right after this, at index 2777 -> crash
            value (get buf i)
            _ (prin "val " value)
            refvalue (get refbuf i)
            _ (prin " ref " refvalue)]
        (unless (= value refvalue)
          (prin " " value " == " refvalue ". "))
        )
      (prin "next.."))))

(end-suite)

