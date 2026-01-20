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
  "Returns a coroutine that yields a range. step is the optional step size. The default step size is 1."
  [from to &opt step]
  (default step 1)
  (if (= step 0)
    (coro)
    (let [cmp-fn (if (> step 0) < >)]
      (coro
        (var i from)
        (while (cmp-fn i to)
          (yield i)
          (set i (+ i step)))))))

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

(defmacro- yield-n-iterables
  ```
  It calls
  (step (f first-element-of-iterable ;first-elements-of-n-iterables)),
  (step (f second-element-of-iterable ;second-elements-of-n-iterables)),
  (step (f third-element-of-iterable ;third-elements-of-n-iterables)),
  and so on until any of the given iterables doesn't have any more element. step should yield a value.
  It is meant to be used by yield-iterables.
  ```
  [n step f iterable iterables]
  # the following comments are macro expansions that help you understand what the macro expands to.
  ~(do
     # (def [iterable0 iterable1 ... iterablen] iterables)
     (def
       ,(seq [k :range [0 n]] (symbol 'iterable k))
       ,iterables)
     # (var key0 nil)
     # (var key1 nil)
     # ...
     # (var keyn nil)
     ,;(seq [k :range [0 n]] ~(var ,(symbol 'key k) nil))
     (each x ,iterable
       # (set key0 (next iterable0 key0))
       # (when (nil? key0) (break))
       # (set key1 (next iterable1 key1))
       # (when (nil? key1) (break))
       # ...
       # (set keyn (next iterablen keyn))
       # (when (nil? keyn) (break))
       ,;;(seq [k :range [0 n]]
            ~[(set ,(symbol 'key k)
                   (next ,(symbol 'iterable k) ,(symbol 'key k)))
              (when (nil? ,(symbol 'key k))
                (break))])
       # (step (f x (in iterable0 key0) (in iterable1 key1) ... (in iterablen keyn)))
       (,step (,f x ,;(seq [k :range [0 n]]
                        ~(in ,(symbol 'iterable k)
                             ,(symbol 'key k))))))))

(defmacro- yield-iterables
  ```
  Returns a coroutine that calls
  (step (f first-element-of-iterable ;first-elements-of-iterables)),
  (step (f second-element-of-iterable ;second-elements-of-iterables)),
  (step (f third-element-of-iterable ;third-elements-of-iterables)),
  and so on until any of the given iterables doesn't have any more element. step should yield a value.
  ```
  [step f iterable iterables]
  ~(coro
     (def niter (length ,iterables))
     (case niter
       0 (each x ,iterable (,step (,f x)))
       1 (yield-n-iterables 1 ,step ,f ,iterable ,iterables)
       2 (yield-n-iterables 2 ,step ,f ,iterable ,iterables)
       3 (yield-n-iterables 3 ,step ,f ,iterable ,iterables)
       (do
         (def keys (array/new-filled niter))
         (def call-buffer (array/new-filled niter))
         (var done false)
         (var old-key nil)
         (var ii nil)
         (var new-key nil)
         (each x ,iterable
           (for i 0 niter
             (set old-key (in keys i))
             (set ii (in ,iterables i))
             (set new-key (next ii old-key))
             (if (nil? new-key)
               (do
                 (set done true)
                 (break))
               (do
                 (put keys i new-key)
                 (put call-buffer i (in ii new-key)))))
           (when done (break))
           (,step (,f x ;call-buffer)))))))

(defmacro- map-step
  [val]
  ~(yield ,val))

(defn map
  ```
  Returns a coroutine that yields
  (f first-element-of-iterable ;first-elements-of-iterables),
  (f second-element-of-iterable ;second-elements-of-iterables),
  (f third-element-of-iterable ;third-elements-of-iterables),
  and so on until any of the given iterables doesn't have any more element.
  ```
  [f iterable & iterables]
  (yield-iterables map-step f iterable iterables))

(defmacro- mapcat-step
  [val]
  ~(each x ,val (yield x)))

(defn mapcat
  ```
  Returns a coroutine that yields
  elements of (f first-element-of-iterable ;first-elements-of-iterables),
  elements of (f second-element-of-iterable ;second-elements-of-iterables),
  elements of (f third-element-of-iterable ;third-elements-of-iterables),
  and so on until any of the given iterables doesn't have any more element.
  ```
  [f iterable & iterables]
  (yield-iterables mapcat-step f iterable iterables))

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

(defn interleave
  ```
  Returns a coroutine that yields the first elements of iterables, and then the second elements of iterables, and then
  the third elements of iterables, and so on until any of the given iterables doesn't have any more element.
  ```
  [iterable & iterables]
  (yield-iterables mapcat-step tuple iterable iterables))
