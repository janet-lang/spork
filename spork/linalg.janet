(use spork/misc)

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
