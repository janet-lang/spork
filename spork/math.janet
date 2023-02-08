(use ./misc)

# Statistics

(defn extent
  "Returns the minimum & maximum number in an `xs` as tuple."
  [xs]
  [(min ;xs) (max ;xs)])

(defn sum-compensated
  "Returns sum of the members of `xs` with Kahan-Babushka algorithm."
  [xs]
  (if (empty? xs)
    0
    (do
      (var s (xs 0))
      (var correction 0)
      (var transition nil)
      (for i 1 (length xs)
        (def x (xs i))
        (set transition (+ s x))
        (if (>= (math/abs s) (math/abs x))
          (+= correction (+ (- s transition) x))
          (+= correction (+ (- x transition) s)))
        (set s transition))
      (+ s correction))))

(defn swap
  ```
  Swaps members with indices `i` and `j` of arr.
  Noop when `i` equals `j`.
  ```
  [arr i j]
  (unless (= i j)
    (def tmp (arr i))
    (put arr i (arr j))
    (put arr j tmp)))

(defn quickselect
  ```
  Rearrange items in `arr` so that all items in `[left, k]` range
  are the smallest.
  The `k`-th element will have the `(k - left + 1)`-th smallest value
  in `[left, right]`.
  Mutates `arr`.
  ```
  [arr k &opt left right]
  (var l (or left 0))
  (var r (or right (dec (length arr))))
  (while (> r l)
    (when (> (- r l) 600)
      (def n (inc (- r l)))
      (def i (inc (- k l)))
      (def z (math/log n))
      (def s (* 0.5 (math/exp (* 2 (/ z 3)))))
      (var sd (* 0.5 (math/sqrt (* z s (/ (- n s) n)))
                 (if (neg? (- i (/ n 2))) -1 1)))
      (def nl (max l (math/floor (+ (- k (* i (/ s n))) sd))))
      (def nr (min r (math/floor (+ k (* (/ s n) (- n i)) sd))))
      (quickselect arr k nl nr))
    (def t (arr k))
    (var i l)
    (var j r)
    (swap arr l k)
    (if (> (arr r) t) (swap arr l r))
    (while (< i j)
      (swap arr i j)
      (++ i)
      (-- j)
      (while (< (arr i) t) (++ i))
      (while (> (arr j) t) (-- j)))
    (if (= (arr l) t)
      (swap arr l j)
      (do
        (++ j)
        (swap arr j r)))

    (if (<= j k) (set l (inc j)))
    (if (<= k j) (set r (dec j)))))

(defn quantile-sorted
  "Gets the quantile value from `xs` at `p` from sorted population."
  [xs p]
  (def idx (* (length xs) p))
  (assert (not (empty? xs))
          "quantile requires at least one data point")
  (assert (<= 0 p 1) "quantile must be between 0 and 1")
  (cond
    (= p 1) (last xs)
    (zero? p) (first xs)
    (not (zero? (mod idx 1))) (xs (dec (math/ceil idx)))
    (zero? (mod (length xs) 2)) (mean [(xs (dec idx)) (xs idx)])
    (xs idx)))

(defn- quantile-index [xsl p]
  (def idx (* xsl p))
  (cond
    (= p 1) (dec xsl)
    (zero? p) 0
    (not (zero? (mod idx 1))) (dec (math/ceil idx))
    (zero? (mod xsl 2)) (- idx 0.5)
    idx))

(defn- quantile-select [xs k left right]
  (if (zero? (mod k 1))
    (quickselect xs k left right)
    (do
      (def fk (math/floor k))
      (quickselect xs fk left right)
      (quickselect xs (inc fk) (inc left) right))))

(defn quantile
  "Gets the quantile value from `xs` at `p` from unsorted population."
  [xs p]
  (def copy (array/slice xs))
  (def cpl (length copy))
  (def idx (quantile-index cpl p))
  (quantile-select copy idx 0 (dec cpl))
  (quantile-sorted copy p))

(defn quantile-rank-sorted
  "Gets the quantile rank of value `v` from sorted `xs`."
  [xs v]
  (defn lower-bound [val]
    (var mid 0)
    (var lo 0)
    (var hi (length xs))
    (while (< lo hi)
      (set mid (brushift (+ lo hi) 1))
      (if (<= v (xs mid))
        (set hi mid)
        (set lo (- (bnot mid)))))
    lo)
  (defn upper-bound [val]
    (var mid 0)
    (var lo 0)
    (var hi (length xs))
    (while (< lo hi)
      (set mid (brushift (+ lo hi) 1))
      (if (>= v (xs mid))
        (set lo (- (bnot mid)))
        (set hi mid)))
    lo)
  (var l 0)
  (var u 0)
  (cond
    (< v (first xs)) 0
    (> v (last xs)) 1
    (do
      (set l (lower-bound v))
      (not= (xs l) v)) (/ l (length xs))
    (do
      (++ l)
      (set u (upper-bound v))
      (= u l)) (/ l (length xs))
    (let [r (inc (- u l))
          s (/ (* r (+ u l)) 2)
          m (/ s r)]
      (/ m (length xs)))))

(defn quantile-rank
  "Gets the quantile rank of value `v` from unsorted `xs`."
  [xs p]
  (quantile-rank-sorted (sorted xs) p))

(defn add-to-mean
  "Adds new value `v` to mean `m` from `n` values."
  [m n v]
  (+ m (/ (- v m) (+ n 1))))

(defn mode
  "Gets the mode value from `xs`."
  [xs]
  (def ifq (invert (frequencies xs)))
  (def k (max ;(keys ifq)))
  (ifq k))

(defn median
  "Gets the median value from `xs`"
  [xs]
  (quantile xs 0.5))

(defn relative-err
  ```
  Gets the relative err between actual number `a`
  and expected number `e`.
  ```
  [a e]
  (if (and (zero? a) (zero? e))
    0 (math/abs (/ (- a e) e))))

(def epsilon "Epsilon constant" 0.0001)

(defn approx-eq
  ```
  Approximate equality between actual number `a`
  and expected number `e`. Default tolerance `t`
  is `epsilon`.
  ```
  [a e &opt t]
  (default t epsilon)
  (<= (relative-err a e) t))

(defn harmonic-mean
  "Gets the harmonic mean from `xs`."
  [xs]
  (assert (not (empty? xs)) "xs cannot be empty")
  (var rs 0)
  (each x xs
    (assert (pos? x) "xs can contain only positive numbers")
    (+= rs (/ 1 x)))
  (/ (length xs) rs))

(defn geometric-mean
  "Gets the geometric mean from `xs`."
  [xs]
  (assert (not (empty? xs)) "xs cannot be empty")
  (var v 1)
  (each x xs
    (assert (pos? x) "xs can contain only positive numbers")
    (*= v x))
  (math/pow v (/ 1 (length xs))))

(defn root-mean-square
  "Gets the root mean square from `xs`."
  [xs]
  (assert (not (empty? xs)) "xs cannot be empty")
  (var sos 0)
  (each x xs
    (+= sos (* x x)))
  (math/sqrt (/ sos (length xs))))

(defn sample-skewness
  "Gets the sample skeweness from the `xs`."
  [xs]
  (assert (> (length xs) 2)
          "xs must have at least three items")
  (def n (length xs))
  (def m (mean xs))
  (var tmp nil)
  (var ssd 0)
  (var scd 0)
  (each x xs
    (set tmp (- x m))
    (+= ssd (* tmp tmp))
    (+= scd (* tmp tmp tmp)))
  (def bc (dec n))
  (def tssd (math/sqrt (/ ssd bc)))
  (def cs (* tssd tssd tssd))
  (/ (* n scd) (* (dec n) (- n 2) cs)))

(defn sum-nth-power-deviations
  "Get the sum of deviations to the n power."
  [xs n]
  (def m (mean xs))
  (var s 0)
  (var tmp nil)
  (each x xs
    (+= s (math/pow (- x m) n)))
  s)

(defn variance
  "Get the variance from the `xs`."
  [xs]
  (assert (not (empty? xs)) "xs cannot be empty")
  (/ (sum-nth-power-deviations xs 2) (length xs)))

(defn sample-variance
  "Get the sample variance from `xs`."
  [xs]
  (assert (> (length xs) 1)
          "xs must have at least two items")
  (def ssdv (sum-nth-power-deviations xs 2))
  (def bc (dec (length xs)))
  (/ ssdv bc))

(defn standard-deviation
  "Gets the standard deviation from `ds`."
  [xs]
  (if (one? (length xs))
    0
    (math/sqrt (variance xs))))

(defn sample-standard-deviation
  "Gets sample standard deviation from `xs`."
  [xs]
  (math/sqrt (sample-variance xs)))

(defn median-absolute-deviation
  "Gets median absolute deviation from `xs`."
  [xs]
  (def m (median xs))
  (def mad @[])
  (each x xs
    (array/push mad (math/abs (- x m))))
  (median mad))

(defn interquartile-range
  "Gets the interquartile range from `xs`."
  [xs]
  (- (quantile xs 0.75) (quantile xs 0.25)))

(defn z-score
  ```
  Gets the standard score for number `x` from mean `m` 
  and standard deviation `d`.
  ```
  [x m d]
  (/ (- x m) d))

(defn sample-covariance
  "Gets the sample covariance between `xs` and `ys`."
  [xs ys]
  (def xl (length xs))
  (assert (= xl (length ys))
          "samples must have equal number of items")
  (assert (> xl 1) "each sample must have at least two items")
  (def xm (mean xs))
  (def ym (mean ys))
  (var s 0)
  (def bc (dec xl))
  (for i 0 xl
    (+= s (* (- (xs i) xm) (- (ys i) ym))))
  (/ s bc))

(defn sample-correlation
  "Gets the sample correlation between `xs` and `ys`."
  [xs ys]
  (/ (sample-covariance xs ys)
     (sample-standard-deviation xs)
     (sample-standard-deviation ys)))

(defn linear-regression
  ```
  Computes the slope `:m` and y-intercept `:b`
  of the function in the struct from set of coordinates.
  ```
  [coords]
  (def cl (length coords))
  (if (one? cl)
    {:m 0 :b (get-in coords [0 1])}
    (do
      (var sx 0)
      (var sy 0)
      (var sxx 0)
      (var sxy 0)
      (each [x y] coords
        (+= sx x)
        (+= sy y)
        (+= sxx (* x x))
        (+= sxy (* x y)))
      {:m (def m
            (/ (- (* cl sxy) (* sx sy))
               (- (* cl sxx) (* sx sx))))
       :b (- (/ sy cl) (/ (* m sx) cl))})))

(defn linear-regression-line
  "Constructs function from struct returned by linear regression."
  [{:m m :b b}]
  (fn linear-regression-line [x]
    (+ b (* m x))))

(defn check-probability
  "Asserts that probability in in the [0 1] range."
  [p]
  (assert (<= 0 p 1) "probability must be between 0 and 1"))

(defn bernoulli-distribution
  "Creates Bernoulli distribution from popability `p` in the tuple."
  [p]
  (check-probability p)
  [(- 1 p) p])

(defn binominal-distribution
  ```
  Creates binominal distribution from trials `t`
  and probability `p` in the tuple.
  ```
  [t p]
  (check-probability p)
  (var x 0)
  (var cp 0)
  (var bc 1)
  (def cells @[])
  (forever
    (set (cells x)
         (*
           bc
           (math/pow p x)
           (math/pow (- 1 p) (- t x))))
    (+= cp (cells x))
    (++ x)
    (set bc (/ (* bc (+ (- t x) 1)) x))
    (if-not (< cp (- 1 epsilon)) (break)))
  (tuple/brackets ;cells))

(defn poisson-distribution
  "Creates Poisson distribution from `lambda` in tuple."
  [lambda]
  (assert (pos? lambda) "lambda must be positive")
  (var x 0)
  (var cp 0)
  (var fx 1)
  (def cells @[])
  (forever
    (set (cells x)
         (/ (* (math/exp (- lambda))
               (math/pow lambda x))
            fx))
    (+= cp (cells x))
    (++ x)
    (*= fx x)
    (if-not (< cp (- 1 epsilon)) (break)))
  (tuple/brackets ;cells))

(defn- cumulative-distribution [z]
  (def sqrt2pi (math/sqrt (* 2 math/pi)))
  (var s z)
  (var t z)
  (loop [i :range [1 15]]
    (*= t (/ (* z z) (inc (* 2 i))))
    (+= s t))
  (/ (math/round
       (* (+ 0.5
             (* (/ s sqrt2pi)
                (math/exp (/ (* (- z) z) 2))))
          10000))
     10000))

(def standard-normal-table
  "Computed standard normal table."
  (do
    (def res @[])
    (loop [z :range-to [0 3.09 0.01]]
      (array/push res (cumulative-distribution z)))
    (tuple/brackets ;res)))

(defn t-test
  ```
  Computes one sample t-test comparing the mean of `xs`
  to known value `expv`.
  ```
  [xs expv]
  (def sm (mean xs))
  (def sd (standard-deviation xs))
  (def rn (math/sqrt (length xs)))
  (/ (- sm expv) (/ sd rn)))

(defn t-test-2
  ```
  Computes two sample t-test of two samples `xs` and `ys`
  with difference optional `d` which defaults to 0.
  ```
  [xs ys &opt d]
  (assert (not (or (empty? xs) (empty? ys)))
          "either sample cannot be empty")
  (default d 0)
  (def n (length xs))
  (def m (length ys))
  (def mx (mean xs))
  (def my (mean ys))
  (def svx (sample-variance xs))
  (def svy (sample-variance ys))
  (def wv (/ (+ (* (dec n) svx)
                (* (dec m) svy))
             (+ n m -2)))
  (/ (- mx my d)
     (math/sqrt (* wv (+ (/ 1 n) (/ 1 m))))))

(defn shuffle-in-place
  ```
  Generate random permutation of the array `xs`
  which is shuffled in place.
  ```
  [xs]
  (var xl (length xs))
  (var t nil)
  (var i nil)
  (while (pos? xl)
    (set i (math/rng-int (math/rng (os/time)) xl))
    (-- xl)
    (set t (xs xl))
    (set (xs xl) (xs i))
    (set (xs i) t))
  xs)

(defn permutation-test
  ```
  Conducts a permutation test to determine if two data sets
  `xs` and `ys` are *significantly* different from each other.
  You can use alternative hypotesis `a`, which defaults to `:two-side`,
  with `:greater` and `:lesser` being the other two options.
  The last optional argument is `k` number of values
  in permutation distribution
  ```
  [xs ys &opt a k]
  (default k 1e4)
  (default a :two-side)
  (def mx (mean xs))
  (def my (mean ys))
  (def ts (- mx my))
  (def tsd @[])
  (def ad (array/concat @[] xs ys))
  (def mi (math/floor (/ (length ad) 2)))
  (for i 0 k
    (shuffle-in-place ad)
    (def pl (array/slice ad 0 mi))
    (def pr (array/slice ad mi))
    (set (tsd i) (- (mean pl) (mean pr))))
  (var nes 0)
  (case a
    :two-side
    (for i 0 k
      (if (>= (math/abs (tsd i)) (math/abs ts))
        (++ nes)))
    :greater
    (for i 0 k
      (if (>= (tsd i) ts)
        (++ nes)))
    :lesser
    (for i 0 k
      (if (<= (tsd i) ts)
        (++ nes))))
  (/ nes k))

(def chi-squared-distribution-table
  "Chi Squared distribution table."
  {1 {0.995 0
      0.99 0
      0.975 0
      0.95 0
      0.9 0.02
      0.5 0.45
      0.1 2.71
      0.05 3.84
      0.025 5.02
      0.01 6.63
      0.005 7.88}
   2 {0.995 0.01
      0.99 0.02
      0.975 0.05
      0.95 0.1
      0.9 0.21
      0.5 1.39
      0.1 4.61
      0.05 5.99
      0.025 7.38
      0.01 9.21
      0.005 10.6}
   3 {0.995 0.07
      0.99 0.11
      0.975 0.22
      0.95 0.35
      0.9 0.58
      0.5 2.37
      0.1 6.25
      0.05 7.81
      0.025 9.35
      0.01 11.34
      0.005 12.84}
   4 {0.995 0.21
      0.99 0.3
      0.975 0.48
      0.95 0.71
      0.9 1.06
      0.5 3.36
      0.1 7.78
      0.05 9.49
      0.025 11.14
      0.01 13.28
      0.005 14.86}
   5 {0.995 0.41
      0.99 0.55
      0.975 0.83
      0.95 1.15
      0.9 1.61
      0.5 4.35
      0.1 9.24
      0.05 11.07
      0.025 12.83
      0.01 15.09
      0.005 16.75}
   6 {0.995 0.68
      0.99 0.87
      0.975 1.24
      0.95 1.64
      0.9 2.2
      0.5 5.35
      0.1 10.65
      0.05 12.59
      0.025 14.45
      0.01 16.81
      0.005 18.55}
   7 {0.995 0.99
      0.99 1.25
      0.975 1.69
      0.95 2.17
      0.9 2.83
      0.5 6.35
      0.1 12.02
      0.05 14.07
      0.025 16.01
      0.01 18.48
      0.005 20.28}
   8 {0.995 1.34
      0.99 1.65
      0.975 2.18
      0.95 2.73
      0.9 3.49
      0.5 7.34
      0.1 13.36
      0.05 15.51
      0.025 17.53
      0.01 20.09
      0.005 21.96}
   9 {0.995 1.73
      0.99 2.09
      0.975 2.7
      0.95 3.33
      0.9 4.17
      0.5 8.34
      0.1 14.68
      0.05 16.92
      0.025 19.02
      0.01 21.67
      0.005 23.59}
   10 {0.995 2.16
       0.99 2.56
       0.975 3.25
       0.95 3.94
       0.9 4.87
       0.5 9.34
       0.1 15.99
       0.05 18.31
       0.025 20.48
       0.01 23.21
       0.005 25.19}
   11 {0.995 2.6
       0.99 3.05
       0.975 3.82
       0.95 4.57
       0.9 5.58
       0.5 10.34
       0.1 17.28
       0.05 19.68
       0.025 21.92
       0.01 24.72
       0.005 26.76}
   12 {0.995 3.07
       0.99 3.57
       0.975 4.4
       0.95 5.23
       0.9 6.3
       0.5 11.34
       0.1 18.55
       0.05 21.03
       0.025 23.34
       0.01 26.22
       0.005 28.3}
   13 {0.995 3.57
       0.99 4.11
       0.975 5.01
       0.95 5.89
       0.9 7.04
       0.5 12.34
       0.1 19.81
       0.05 22.36
       0.025 24.74
       0.01 27.69
       0.005 29.82}
   14 {0.995 4.07
       0.99 4.66
       0.975 5.63
       0.95 6.57
       0.9 7.79
       0.5 13.34
       0.1 21.06
       0.05 23.68
       0.025 26.12
       0.01 29.14
       0.005 31.32}
   15 {0.995 4.6
       0.99 5.23
       0.975 6.27
       0.95 7.26
       0.9 8.55
       0.5 14.34
       0.1 22.31
       0.05 25
       0.025 27.49
       0.01 30.58
       0.005 32.8}
   16 {0.995 5.14
       0.99 5.81
       0.975 6.91
       0.95 7.96
       0.9 9.31
       0.5 15.34
       0.1 23.54
       0.05 26.3
       0.025 28.85
       0.01 32
       0.005 34.27}
   17 {0.995 5.7
       0.99 6.41
       0.975 7.56
       0.95 8.67
       0.9 10.09
       0.5 16.34
       0.1 24.77
       0.05 27.59
       0.025 30.19
       0.01 33.41
       0.005 35.72}
   18 {0.995 6.26
       0.99 7.01
       0.975 8.23
       0.95 9.39
       0.9 10.87
       0.5 17.34
       0.1 25.99
       0.05 28.87
       0.025 31.53
       0.01 34.81
       0.005 37.16}
   19 {0.995 6.84
       0.99 7.63
       0.975 8.91
       0.95 10.12
       0.9 11.65
       0.5 18.34
       0.1 27.2
       0.05 30.14
       0.025 32.85
       0.01 36.19
       0.005 38.58}
   20 {0.995 7.43
       0.99 8.26
       0.975 9.59
       0.95 10.85
       0.9 12.44
       0.5 19.34
       0.1 28.41
       0.05 31.41
       0.025 34.17
       0.01 37.57
       0.005 40}
   21 {0.995 8.03
       0.99 8.9
       0.975 10.28
       0.95 11.59
       0.9 13.24
       0.5 20.34
       0.1 29.62
       0.05 32.67
       0.025 35.48
       0.01 38.93
       0.005 41.4}
   22 {0.995 8.64
       0.99 9.54
       0.975 10.98
       0.95 12.34
       0.9 14.04
       0.5 21.34
       0.1 30.81
       0.05 33.92
       0.025 36.78
       0.01 40.29
       0.005 42.8}
   23 {0.995 9.26
       0.99 10.2
       0.975 11.69
       0.95 13.09
       0.9 14.85
       0.5 22.34
       0.1 32.01
       0.05 35.17
       0.025 38.08
       0.01 41.64
       0.005 44.18}
   24 {0.995 9.89
       0.99 10.86
       0.975 12.4
       0.95 13.85
       0.9 15.66
       0.5 23.34
       0.1 33.2
       0.05 36.42
       0.025 39.36
       0.01 42.98
       0.005 45.56}
   25 {0.995 10.52
       0.99 11.52
       0.975 13.12
       0.95 14.61
       0.9 16.47
       0.5 24.34
       0.1 34.28
       0.05 37.65
       0.025 40.65
       0.01 44.31
       0.005 46.93}
   26 {0.995 11.16
       0.99 12.2
       0.975 13.84
       0.95 15.38
       0.9 17.29
       0.5 25.34
       0.1 35.56
       0.05 38.89
       0.025 41.92
       0.01 45.64
       0.005 48.29}
   27 {0.995 11.81
       0.99 12.88
       0.975 14.57
       0.95 16.15
       0.9 18.11
       0.5 26.34
       0.1 36.74
       0.05 40.11
       0.025 43.19
       0.01 46.96
       0.005 49.65}
   28 {0.995 12.46
       0.99 13.57
       0.975 15.31
       0.95 16.93
       0.9 18.94
       0.5 27.34
       0.1 37.92
       0.05 41.34
       0.025 44.46
       0.01 48.28
       0.005 50.99}
   29 {0.995 13.12
       0.99 14.26
       0.975 16.05
       0.95 17.71
       0.9 19.77
       0.5 28.34
       0.1 39.09
       0.05 42.56
       0.025 45.72
       0.01 49.59
       0.005 52.34}
   30 {0.995 13.79
       0.99 14.95
       0.975 16.79
       0.95 18.49
       0.9 20.6
       0.5 29.34
       0.1 40.26
       0.05 43.77
       0.025 46.98
       0.01 50.89
       0.005 53.67}
   40 {0.995 20.71
       0.99 22.16
       0.975 24.43
       0.95 26.51
       0.9 29.05
       0.5 39.34
       0.1 51.81
       0.05 55.76
       0.025 59.34
       0.01 63.69
       0.005 66.77}
   50 {0.995 27.99
       0.99 29.71
       0.975 32.36
       0.95 34.76
       0.9 37.69
       0.5 49.33
       0.1 63.17
       0.05 67.5
       0.025 71.42
       0.01 76.15
       0.005 79.49}
   60 {0.995 35.53
       0.99 37.48
       0.975 40.48
       0.95 43.19
       0.9 46.46
       0.5 59.33
       0.1 74.4
       0.05 79.08
       0.025 83.3
       0.01 88.38
       0.005 91.95}
   70 {0.995 43.28
       0.99 45.44
       0.975 48.76
       0.95 51.74
       0.9 55.33
       0.5 69.33
       0.1 85.53
       0.05 90.53
       0.025 95.02
       0.01 100.42
       0.005 104.22}
   80 {0.995 51.17
       0.99 53.54
       0.975 57.15
       0.95 60.39
       0.9 64.28
       0.5 79.33
       0.1 96.58
       0.05 101.88
       0.025 106.63
       0.01 112.33
       0.005 116.32}
   90 {0.995 59.2
       0.99 61.75
       0.975 65.65
       0.95 69.13
       0.9 73.29
       0.5 89.33
       0.1 107.57
       0.05 113.14
       0.025 118.14
       0.01 124.12
       0.005 128.3}
   100 {0.995 67.33
        0.99 70.06
        0.975 74.22
        0.95 77.93
        0.9 82.36
        0.5 99.33
        0.1 118.5
        0.05 124.34
        0.025 129.56
        0.01 135.81
        0.005 140.17}})

(defn cumulative-std-normal-probability
  "Computes standard normal probability for `y`."
  [z]
  (def i (min (math/round (* 100 (math/abs z)))
              (dec (length standard-normal-table))))
  (if (neg? z)
    (- 1 (standard-normal-table i))
    (standard-normal-table i)))

(defn factorial
  "Returns factorial of `n`."
  [n]
  (if (zero? n) 1 (product (range 1 (inc n)))))

(defn binominal-coeficient
  ```
  Computes binominal coeficient from set of size `n`
  and sample size `k`.
  ```
  [n k]
  (cond
    (= n k) 1
    (< n k) 0
    (do-var
      c 1
      (def nk (min k (- n k)))
      (for i 0 nk (*= c (/ (- n i) (inc i)))))))

(defn permutations
  "Returns permutations of length `k` from members of `s`"
  [s &opt k]
  (default k (length s))
  (def c (array/new-filled k 0))
  (do-var
    res @[]
    (array/push res (array/slice s))
    (var i 0)
    (while (< i k)
      (if (< (c i) i)
        (do
          (if (even? k)
            (swap s 0 i)
            (swap s (c i) i))
          (array/push res (array/slice s))
          (update c i inc)
          (set i 0))
        (do
          (put c i 0)
          (++ i))))))

# Linear Algebra

(defmacro get-only-el
  ```
  Convenience macro for geting first element
  from first row of the two dimensional array `m`.
  ```
  [m]
  ~(in (in ,m 0) 0))

(defn rows
  "Returns numbers of rows of matrix `m`."
  [m]
  (length m))

(defn cols
  "Returns numbers of columns of matrix `m`."
  [m]
  (length (m 0)))

(defn size
  "Returns tuple with the matrix `m` size [rows cols]."
  [m]
  [(rows m) (cols m)])

(defn zero
  ```
  Creates vector of length `c`, or matrix if `r`
  is provided, and fills it with zeros.
  ```
  [c &opt r]
  (def v (array/new-filled c 0))
  (if r
    (seq [_ :range [0 c]] (array/slice v))
    v))

(defn scalar
  "Creates scalar `s` matrix with `c` x `c` size."
  [c s]
  (def m (array/new c))
  (for i 0 c
    (def r (array/new c))
    (for j 0 c
      (array/push r (if (= i j) s 0)))
    (array/push m r))
  m)

(defn ident
  "Creates identity matrix with `c` x `c` size."
  [c]
  (scalar c 1))

(defn trans
  "Returns a new transposed matrix from `m`."
  [m]
  (def [c r] (size m))
  (def res (array/new c))
  (for i 0 r
    (def cr (array/new c))
    (for j 0 c
      (array/push cr (get-in m [j i])))
    (array/push res cr))
  res)

(defn sop
  ```
  Mutates every cell of the matrix `m` with `op`
  and variadic args `a`.
  ```
  [m op & a]
  (def opa
    (if-not (empty? a) |(op $ ;a) op))
  (for i 0 (cols m)
    (for j 0 (rows m)
      (update-in m [i j] opa))))

(defn mop
  ```
  Mutates every cell of the matrix `m` with `op`
  and corresponding cell from matrix arg `a`.
  ```
  [m op a]
  (for i 0 (cols m)
    (for j 0 (rows m)
      (update-in m [j i] op (get-in a [j i])))))

(defn add
  ```
  Add `a` to matrix `m` where it can be matrix or scalar.
  Matrix `m` is mutated.
  ```
  [m a]
  (case (type a)
    :number (sop m + a)
    :array (mop m + a)))

(defn dot
  "Computes dot product of matrices or vectors `x` and `y`."
  [mx my]
  (def [rx cx] (size mx))
  (def [ry cy] (size my))
  (assert (= cx ry) "matrices do not have right sizes for dot product")
  (def res (array/new cy))
  (for r 0 rx
    (def cr (array/new cx))
    (for c 0 cy
      (var s 0)
      (for rr 0 ry
        (+= s (* (get-in mx [r rr]) (get-in my [rr c]))))
      (array/push cr s))
    (array/push res cr))
  res)

(defn mul
  ```
  Multiply matrix `m` with `a` which can be matrix or vector.
  Matrix `m` is mutated.
  ```
  [m a]
  (case (type a)
    :number
    (sop m * a)
    :array
    (if (number? (a 0))
      (dot m (seq [x :in a] @[x]))
      (mop m * a))))

(defn minor
  "Computes minor matrix of matrix `m` and `x`, `y`."
  [m x y]
  (def dn (dec (rows m)))
  (def res
    (seq [_ :range [0 dn]]
      (array/new dn)))
  (loop [i :range [0 dn]
         j :range [0 dn]]
    (def coord
      (cond
        (and (< i x) (< j y)) [i j]
        (and (>= i x) (< j y)) [(inc i) j]
        (and (< i x) (>= j y)) [i (inc j)]
        [(inc i) (inc j)]))
    (put-in res [i j] (get-in m coord)))
  res)

(defmacro- check-square->rows [m]
  (with-syms [r c]
    ~(do
       (def [,r ,c] (size ,m))
       (assert (= ,r ,c) "matrix must a be square")
       ,r)))

(defn det
  "Computes determinant of matrix `m`."
  [m]
  (def r (check-square->rows m))
  (if (one? r)
    (get-only-el m)
    (do-var res 0
            (var sign 1)
            (for i 0 r
              (+= res (* sign
                         (get-in m [0 i])
                         (det (minor m 0 i))))
              (*= sign -1)))))

(defn perm
  "Computes permanent of the matrix `m`."
  [m]
  (def r (check-square->rows m))
  (if (one? r)
    (get-only-el m)
    (do-var res 0
            (for i 0 r
              (+= res (* (get-in m [0 i])
                         (perm (minor m 0 i))))))))
