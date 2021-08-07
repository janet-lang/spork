(defn from-iterable
  "Create a new stream around any iterable data structure."
  [ds]
  (coro (each x ds (yield x))))

(defn range
  "Create a lazy range."
  [from to]
  (coro (for i from to (yield i))))

(defn to-array
  ``Consume `s` into a new array.

    NB: this will create an infinite loop if `s` is an infinite stream!``
  [s]
  (values s))

(defn run
  ``Evaluate `s` for side effects.

    NB: this will create an infinite loop if `s` is an infinite stream!``
  [s]
  (each _ s))

(defn concat
  "Concatenate one or more streams or iterables into a single stream."
  [& xs]
  (coro (each x xs
          (each elem x (yield elem)))))

(defn map
  "Create a stream that maps `f` over `ds`."
  [f ds]
  (coro (each x ds (yield (f x)))))

(defn mapcat
  "Map `f` over `ds`, concatenating the results into a new stream."
  [f ds]
  (coro (each x ds
          (each elem (f x) (yield elem)))))

(defn filter
  "Create a stream that filters `ds` with `p`."
  [p ds]
  (coro (each x ds (if (p x) (yield x)))))

(defn take
  "Take `n` elements from iterable `ds`."
  [n ds]
  (coro
    (var taken 0)
    (each x ds
      (yield x)
      (+= taken 1)
      (if (= taken n)
        (break)))))

(defn take-while
  "Return elements from `ds` while `p` is true."
  [p ds]
  (coro (each x ds
          (if (p x)
            (yield x)
            (break)))))

(defn take-until
  "Return elements from `ds` until `p` is true."
  [p ds]
  (take-while (complement p) ds))

(defn drop
  "Drop `n` elements from `ds`."
  [n ds]
  (coro
    (var dropped 0)
    (each x ds
      (if (= dropped n)
        (yield x)
        (+= dropped 1)))))

(defn drop-while
  "Drop elements from `ds` while `p` is true."
  [p ds]
  (coro
    (var dropping true)
    (each x ds
      (if (and dropping (p x))
        nil
        (do
          (set dropping false)
          (yield x))))))

(defn drop-until
  "Drop elements from `ds` until `p` is true."
  [p ds]
  (drop-while (complement p) ds))

(defn cycle
  "Repeatedly yield the elements of `ds`, looping back to the beginning when finished."
  [ds]
  (coro
    (def to-cycle (if (indexed? ds) ds @[ds]))
    (var i nil)
    (while true
      (set i (next to-cycle i))
      (if (nil? i) (set i 0))
      (yield (to-cycle i)))))
