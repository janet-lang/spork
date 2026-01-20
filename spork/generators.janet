###
### generators.janet
###
### Module for fiber-based sequence combinators rather than array-based combinators in the core library.
###

(defn from-iterable
  "Returns a coroutine that yields elements of `iterable`."
  [iterable]
  (coro (each x iterable (yield x))))

(defn range
  "Create a lazy range."
  [from to]
  (coro (for i from to (yield i))))

(defn to-array
  "Collect `iterable` elements into a new array."
  [iterable]
  (seq [v :in iterable] v))

(defn run
  "Evaluate `iterable` for potential side effects."
  [iterable]
  (each _ iterable))

(defn concat
  ```
  Returns a coroutine that yields elements of the first given iterable, and then elements of the second given iterable,
  and so on until elements of the last given iterable are yielded.
  ```
  [& iterables]
  (coro
    (each iterable iterables
      (each elem iterable (yield elem)))))

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
  "Returns a coroutine that yields only `iterable` elements for which `(pred element)` is truthy."
  [pred iterable]
  (coro (each e iterable (if (pred e) (yield e)))))

(defn take
  "Returns a coroutine that yields the first `n` elements from `iterable`."
  [n iterable]
  (coro
    (var taken 0)
    (each x iterable
      (yield x)
      (+= taken 1)
      (if (= taken n)
        (break)))))

(defn take-while
  "Returns a coroutine that yields `iterable` elements while `(pred element)` is truthy."
  [pred iterable]
  (coro (each x iterable
          (if (pred x)
            (yield x)
            (break)))))

(defn take-until
  "Returns a coroutine that yields `iterable` elements until `(pred element)` becomes truthy."
  [pred iterable]
  (take-while (fn :pred-in-take-until [x] (not (pred x))) iterable))

(defn drop
  "Returns a coroutine that drops the first `n` elements from `iterable` and yields the rest of the elements."
  [n iterable]
  (coro
    (var dropped 0)
    (each x iterable
      (if (= dropped n)
        (yield x)
        (+= dropped 1)))))

(defn drop-while
  "Returns a coroutine that drops `iterable` elements while `(pred element)` is truthy."
  [pred iterable]
  (coro
    (var dropping true)
    (each x iterable
      (if (and dropping (pred x))
        nil
        (do
          (set dropping false)
          (yield x))))))

(defn drop-until
  "Return a coroutine that drops `iterable` elements until `(pred element)` becomes truthy."
  [pred iterable]
  (drop-while (fn :pred-in-drop-until [x] (not (pred x))) iterable))

(defn cycle
  ```
  Returns a coroutine that yields `iterable` elements and repetitively loops back to the beginning when finished.
  If `iterable` is a fiber, cycle cannot loop back to the beginning. Don't pass a fiber to cycle.
  ```
  [iterable]
  (coro
    (var i nil)
    (while true
      (set i (next iterable i))
      (if (nil? i) (set i (next iterable)))
      (yield (in iterable i)))))
