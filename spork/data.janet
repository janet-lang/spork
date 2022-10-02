###
### data.janet 
###
### Compare data structures using `diff`.
###

(varfn diff [])

(defn- atom-diff
  [a b]
  (if (= a b) @[nil nil a] @[a b nil]))

(defn- in? [x ds]
  (if (index-of x ds)
    true
    false))

(defn- safe-in [ds n]
  (if (in? (type ds) [:array :tuple])
    (if (<= n (dec (length ds)))
      (in ds n)
      nil)
    (in ds n)))

(defn- arr [a]
  (apply array a))

(defn- vectorize [m]
  (unless (or (nil? m) (empty? m))
    (when (in? (type m) [:array :tuple :table :struct])
      (reduce
        (fn [result [k v]] (put result k v))
        (array/new-filled (apply max (keys m)))
        (pairs m)))))

(defn- diff-associative-key [a b k]
  (let [va (safe-in a k)
        vb (safe-in b k)
        [a* b* ab] (diff va vb)
        in-a (in? k (keys a))
        in-b (in? k (keys b))
        same (and in-a in-b
                  (or (not (nil? ab))
                      (and (nil? va) (nil? vb))))]
    [(when (and in-a (or (not (nil? a*)) (not same))) {k a*})
     (when (and in-b (or (not (nil? b*)) (not same))) {k b*})
     (when same {k ab})]))

(defn- diff-associative [a b ks]
  (reduce
    (fn [diff1 diff2]
      (map |(if (empty? $) nil $)
           (map |(merge (or $0 {}) (or $1 {})) diff1 diff2)))
    [nil nil nil]
    (map
      (partial diff-associative-key a b)
      ks)))

(defn- diff-sequential [a b]
  (map vectorize (diff-associative
                   (if (array? a) a (arr a))
                   (if (array? b) b (arr b))
                   (range (max (length a) (length b))))))

(defn- diff-similar [kind a b]
  (cond
    (in? kind [:array :tuple]) (diff-sequential a b)
    (in? kind [:table :struct]) (diff-associative a b (distinct (array/concat (keys a) (keys b))))
    (atom-diff a b)))

(defn- categorize [x]
  (cond
    (in? (type x) [:array :tuple]) :sequence
    (in? (type x) [:table :struct]) :associative
    :atom))

(varfn diff
  ``` 
  Compares a and b recursively. Returns an array of 
  [things-only-in-a things-only-in-b things-in-both].   
  ```
  [a b]
  (if (= a b)
    @[nil nil (cond (tuple? a) (apply array a)
                (struct? a) (struct/to-table a) a)]
    (if (= (categorize a) (categorize b))
      (diff-similar (type a) a b)
      (atom-diff a b))))
