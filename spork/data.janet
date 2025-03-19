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

(defn- vectorize [m]
  (unless (or (nil? m) (empty? m))
    (when (in? (type m) [:array :tuple :table :struct])
      (reduce
        (fn [result [k v]] (put result k v))
        (array/new-filled (max ;(keys m)))
        (pairs m)))))

(defn- diff-associative-key [a b k]
  (let [va (safe-in a k)
        vb (safe-in b k)
        [a* b* ab] (diff va vb)
        same (and (in? k (keys a)) (in? k (keys b))
                  (or (not (nil? ab))
                      (and (nil? va) (nil? vb))))]
    [(when (and (in? k (keys a)) (or (not (nil? a*)) (not same))) {k a*})
     (when (and (in? k (keys b)) (or (not (nil? b*)) (not same))) {k b*})
     (when same {k ab})]))

(defn- diff-associative [a b &opt ks]
  (default ks (distinct (array/concat (keys a) (keys b))))
  (let [reduced (reduce
                  (fn [diff1 diff2]
                    (map |(if (empty? $) nil $)
                         (map |(merge (or $0 @{}) (or $1 @{})) diff1 diff2)))
                  [nil nil nil]
                  (map (partial diff-associative-key a b) ks))]
    reduced))

(defn- diff-sequential [a b]
  (map vectorize (diff-associative
                   (if (array? a) a (array ;a))
                   (if (array? b) b (array ;b))
                   (range (max (length a) (length b))))))

(varfn diff
  ``` 
  Compares a and b recursively. Returns an array of 
  @[things-only-in-a things-only-in-b things-in-both].   
  ```
  [a b]
  (if (deep= a b)
    @[nil nil (postwalk |(cond (tuple? $) (array ;$)
                           (struct? $) (struct/to-table $) $) a)]
    (do
      (cond
        (all indexed? [a b]) (diff-sequential a b)
        (all dictionary? [a b]) (diff-associative a b)
        (atom-diff a b)))))
