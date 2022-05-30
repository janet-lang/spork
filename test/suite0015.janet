(use ../spork/test)
(import spork/tarray)

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

(end-suite)
