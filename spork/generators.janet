###
### generators.janet
###
### Module for fiber based sequence combinators rather than array based combinators, as
### are in the core library.
###

(defn from-iterable
  "Create a new generator around any iterable data structure."
  [ds]
  (coro (each x ds (yield x))))

(defn range
  "Create a lazy range."
  [from to]
  (coro (for i from to (yield i))))

(defn to-array
  "Consume `s` into a new array."
  [s]
  (seq [v :in s] v))

(defn run
  "Evaluate `s` for side effects."
  [s]
  (each _ s))

(defn concat
  "Concatenate one or more generators or iterables into a single generator."
  [& xs]
  (coro (each x xs
          (each elem x (yield elem)))))

(defn map
  "Create a generator that maps `f` over `ds`."
  [f ds]
  (coro (each x ds (yield (f x)))))

(defn mapcat
  "Map `f` over `ds`, concatenating the results into a new generator."
  [f ds]
  (coro (each x ds
          (each elem (f x) (yield elem)))))

(defn filter
  "Create a generator that filters `ds` with `p`."
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
    (var i nil)
    (while true
      (set i (next ds i))
      (if (nil? i) (set i (next ds)))
      (yield (ds i)))))
