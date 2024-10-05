###
### misc.janet
###
### One-off functions that don't need their own module.
###

(import spork/rawterm)

(defn dedent
  ```
  Remove indentation after concatenating the arguments. Works by removing
  leading whitespace, and then removing that same pattern of whitespace after
  new lines.
  ```
  [& xs]
  (def x (string ;xs))
  (def first-letter (find-index (fn [c] (and (not= c (chr "\n"))
                                             (not= c (chr " "))
                                             (not= c (chr "\t"))
                                             (not= c (chr "\r")))) x))
  (if (not first-letter) (break ""))
  (def leading-whitespace (string/slice x 0 first-letter))
  (def indent (last (string/split "\n" leading-whitespace)))
  (if (and indent (not= indent ""))
    (let [y (string/replace-all (string "\n" indent)
                                "\n" (string/replace indent "" x))]
      # Remove trailing newline to mimic long string newline omission.
      (if (= (chr "\n") (last y))
        (slice y 0 (dec (length y)))
        y))
    x))

(defmacro set*
  ```
  Parallel `set` function.  Takes a list of targets and
  expressions, evaluates all the expressions, and then
  assigns them to the targets.  Each target can be a variable
  or a 2-tuple, just like in the normal `set` special form.
  ```
  [tgts exprs]
  (when (not= (length tgts) (length exprs))
    (error "Expected tgts and exprs to have same length"))
  (def code @['do])
  (def syms @[])
  (loop [e :in exprs]
    (def sym (gensym))
    (array/push syms sym)
    (array/push code (tuple 'def sym e)))
  (loop [[i t] :pairs tgts]
    (array/push code (tuple 'set t (in syms i))))
  (tuple ;code))

(defn format-table
  ```
  Same as print-table but pushes table into a buffer.
  ```
  [buf-into data &opt columns header-mapping column-mapping]
  (var colkeys columns)
  (def column-widths @{})
  (def processed @[])
  (default header-mapping {})
  (default column-mapping {})
  (def pass-through (fn [x _] x))

  # Get columns if not provided.
  (unless colkeys
    (def all-keys @{})
    (each row data
      (eachk k row (put all-keys k true)))
    (set colkeys (sorted (keys all-keys))))

  # preprocess rows
  (each row data
    (def newrow @[])
    (each key colkeys
      (def process (get column-mapping key pass-through))
      (def item (string (process (get row key) row)))
      (set (column-widths key) (max (rawterm/monowidth item) (get column-widths key 0)))
      (array/push newrow item))
    (array/push processed newrow))

  # apply width of header names
  (def final-widths @[])
  (each key colkeys
    (def header (get header-mapping key key))
    (def final-width (max (rawterm/monowidth header) (get column-widths key 0)))
    (array/push final-widths final-width)
    (set (column-widths key) final-width))
  (def max-cell-width (extreme > final-widths))

  # build horizontal bars
  (def topbuf (or buf-into @""))
  (buffer/push topbuf "╭")
  (def hbuf @"│")
  (def midbuf @"╞")
  (def botbuf @"╰")
  (each key colkeys
    (def header (get header-mapping key key))
    (def len (rawterm/monowidth header))
    (def width (get column-widths key))
    (def whitespace (- width len))
    (def before (math/ceil (* 0.5 whitespace)))
    (def after (- whitespace before))
    (def bar (string/repeat "─" width))
    (def dbar (string/repeat "═" width))
    (buffer/push topbuf bar "┬")
    (buffer/push botbuf bar "┴")
    (buffer/push midbuf dbar "╪")
    (repeat before (buffer/push hbuf " "))
    (buffer/push hbuf header)
    (repeat after (buffer/push hbuf " "))
    (buffer/push hbuf "│"))
  (buffer/popn topbuf 3)
  (buffer/popn midbuf 3)
  (buffer/popn botbuf 3) # 3 bytes, 1 char
  (buffer/push topbuf (string "╮\n"))
  (buffer/push midbuf (string "╡\n"))
  (buffer/push botbuf (string "╯\n"))
  (buffer/push hbuf "\n")

  # build large buffer
  (buffer/push topbuf hbuf midbuf)
  (each row processed
    (buffer/push topbuf "│")
    (forv i 0 (length row)
      (def item (in row i))
      (def width (in final-widths i))
      (def len (rawterm/monowidth item))
      (buffer/push
        topbuf
        (string/repeat " " (- width len))
        item
        "│"))
    (buffer/push topbuf "\n"))
  (buffer/push topbuf botbuf)
  topbuf)


(defn print-table
  ```
  Iterate through the rows of a data structure and print a table in a human
  readable way, with padding and heading information. Can optionally provide
  a function used to print a row, as well as optionally select column keys
  for each row. Lastly, a `header-mapping` dictionary can be provided that
  changes the printed header names by mapping column keys to the desired
  header name. If no mapping is found, then the column key will be used as
  the header name. Returns nil.
  ```
  [data &opt columns header-mapping column-mapping]
  (print (format-table @"" data columns header-mapping column-mapping)))

(defn- default-traversal-predicate
  [x]
  (def has-kids
    (in
      {:array true
       :tuple true
       :struct true
       :table true
       :fiber true}
      (type x)))
  (if has-kids x))

(defn dfs
  ```
  Do a depth first, pre-order traversal over a data structure.
  Also allow for callbacks before and after visiting the children
  of a node. Also allow for a custom `get-children` function to
  change traversal as needed. Will detect cycles if an empty table
  is passed as the `seen` parameter, which is used to cache values
  that have been visited.
  ```
  [data visit-leaf &opt node-before node-after get-children seen]
  (default get-children default-traversal-predicate)
  (when seen
    (if (in seen data) (break))
    (put seen data true))
  (if-let [node (get-children data)]
    (do
      (when node-before (node-before node))
      (each child node
        (dfs child visit-leaf node-before node-after get-children seen))
      (when node-after (node-after node)))
    (visit-leaf data)))

(defn randomize-array
  ```
  Randomizes array using the fisher-yates shuffle, takes an optional random
  number generator.
  ```
  [arr &opt rng]
  (default rng (math/rng (os/cryptorand 8)))
  (def l (length arr))
  (loop [i :range [0 l]]
    (def a (- (- l i) 1))
    (def b (math/rng-int rng (+ a 1)))
    (def tmp (arr a))
    (put arr a (arr b))
    (put arr b tmp))
  arr)

(defn trim-prefix
  "Trim the specified prefix of a string if it has one"
  [prefix str]
  (if (string/has-prefix? prefix str)
    (slice str (length prefix))
    str))

(defn trim-suffix
  "Trim the specified suffix of a string if it has one"
  [suffix str]
  (if (string/has-suffix? suffix str)
    (slice str 0 (- (length str) (length suffix)))
    str))

(defmacro log
  ```
  Print to a dynamic binding stream if that stream is set, otherwise do
  nothing. Evaluate to nil.
  For example, `(log :err "value error: %V" my-value)` will print
  to `(dyn :err)` only if `(dyn :err)` has been set.
  ```
  [level & args]
  (def to (gensym))
  ~(when-let [,to (,dyn ,level)]
     (,xprintf ,to ,;args)))

(defn map-keys
  ```
  Returns new table with function `f` applied to `data`'s
  keys recursively.
  ```
  [f data]
  (def res @{})
  (loop [[k v] :pairs data]
    (put res (f k)
         (if (dictionary? v)
           (map-keys f v) v)))
  res)

(defn map-keys-flat
  ```
  Returns new table with function `f` applied to `data`'s
  keys without recursing.
  ```
  [f data]
  (tabseq [[k v] :pairs data]
    (f k) v))

(defn map-vals
  "Returns new table with function `f` applied to `data`'s values."
  [f data]
  (def res @{})
  (loop [[k v] :pairs data] (put res k (f v)))
  res)

(defn select-keys
  "Returns new table with selected `keyz` from dictionary `data`."
  [data keyz]
  (def res @{})
  (loop [k :in keyz :when (data k)]
    (put res k (data k)))
  res)

(defmacro cond->
  ```
  Threading conditional macro. It takes `val` to mutate,
  and `clauses` pairs with condition and operation to which `val`,
  is passed as first argument. All conditions are tried and
  for truthy conditions the operation is executed.
  Returns the value mutated if any condition is truthy.
  ```
  [val & clauses]
  (with-syms [res]
    ~(do
       (var ,res ,val)
       ,;(map
           (fn [[cnd ope]]
             (def ope (if (tuple? ope) ope (tuple ope)))
             (tuple
               'if cnd
               (tuple 'set res
                      (tuple (first ope) res
                             ;(tuple/slice ope 1)))))
           (partition 2 clauses))
       ,res)))

(defmacro cond->>
  ```
  Threading conditional macro. It takes `val` to mutate,
  and `clauses` pairs of condition and operation to which `val`,
  is passed as last argument. All conditions are tried and
  for truthy conditions the operation is executed.
  Returns mutated value if any condition is truthy.
  ```
  [val & clauses]
  (with-syms [res]
    ~(do
       (var ,res ,val)
       ,;(map
           (fn [[cnd ope]]
             (def ope (if (tuple? ope) ope (tuple ope)))
             (tuple
               'if cnd
               (tuple 'set res (tuple ;ope res))))
           (partition 2 clauses))
       ,res)))

(defmacro make
  ```
  Convenience macro for creating new table from even number of kvs pairs in variadic `pairs`
  arguments and setting its prototype to `prototype`.
  Factory function for creating new objects from prototypes.
  ```
  [prototype & pairs]
  ~(,table/setproto (,table ,;pairs) ,prototype))

(defmacro do-var
  ```
  Convenience macro for defining variable named `v` with value `d` before `body`
  and returning it after evaluating `body`, that presumably modifies `v`.
  ```
  [v d & body]
  ~(do (var ,v ,d) ,;body ,v))

(defmacro do-def
  ```
  Convenience macro for defining constant named `c` with value `d` before `body`
  and returning it after evaluating `body`, that presumably modifies
  the content referred to by `c`. For example, a buffer, table or array.
  ```
  [c d & body]
  ~(do (def ,c ,d) ,;body ,c))

(defmacro- cap*
  [out & body]
  (with-syms [o]
    ~(as-macro ,do-var ,o @""
               (with-dyns [,out ,o] ,;body))))

(defmacro capout
  ```
  Captures the standard output of the variadic `body` and returns it as
  a buffer.
  ```
  [& body]
  ~(as-macro ,cap* :out ,;body))

(defmacro caperr
  ```
  Captures the standard error output of the variadic `body` and returns it
  as a buffer.
  ```
  [& body]
  ~(as-macro ,cap* :err ,;body))

(defmacro vars
  "Defines many variables as in let `bindings`, but without creating a new scope."
  [& bindings]
  ~(upscope
     ,;(seq [[n v] :in (partition 2 bindings)] (tuple 'var n v))))

(defmacro defs
  "Defines many constants as in let `bindings`, but without creating a new scope."
  [& bindings]
  ~(upscope
     ,;(seq [[n v] :in (partition 2 bindings)] (tuple 'def n v))))

(defn always
  "Return a function that discards any arguments and always returns `x`."
  [x]
  (fn [&] x))

(defn second
  "Get the second element from an indexed data structure."
  [xs]
  (get xs 1))

(defn third
  "Get the third element from an indexed data structure."
  [xs]
  (get xs 2))

(defn penultimate
  "Get the second-to-last element from an indexed data structure."
  [xs]
  (get xs (- (length xs) 2)))

(defn antepenultimate
  "Get the third-to-last element from an indexed data structure."
  [xs]
  (get xs (- (length xs) 3)))

(defn int/
  "Perform integer division."
  [& xs]
  (math/trunc (/ ;xs)))

(defmacro gett
  "Recursive macro (get). Similar to get-in, but `keyz` is variadic."
  [ds & keyz]
  (reduce (fn [t key] (tuple get t key)) ds keyz))

(defmacro until
  ```
  Repeat `body` while the `cnd` is false.
  Equivalent to (while (not cnd) ;body).
  ```
  [cnd & body]
  ~(while (not ,cnd) ,;body))

(defn table-filter
  ```
  Filter a key-value structure into a table. Semantics are the same as for
  built-in `filter`, except that `pred` takes two arguments (key and value).
  Does not consider prototypes.
  ```
  [pred dict]
  (->> dict
       (pairs)
       (filter (fn [[k v]] (pred k v)))
       (from-pairs)))

(def- int-alphabet "0123456789abcdefghijklmnopqrstuvwxyz")

(defn string->int
  ```
  Parse an integer in the given base. Defaults to decimal (base 10). Differs
  from scan-number in that this does not recognize floating point notation.
  ```
  [str &opt base]
  (default base 10)
  (if-not (<= 2 base (length int-alphabet))
    (error "invalid base"))
  (def usable-alphabet (slice int-alphabet 0 base))
  (var value 0)
  (each char str
    (def char
      (if (<= (chr "A") char (chr "Z"))
        (+ char (- (chr "a") (chr "A")))
        char))
    (def digit (index-of char int-alphabet))
    (if (or (nil? digit) (> digit base))
      (error "malformed integer"))
    (set value (+ (* value base) digit)))
  value)

(defn int->string
  "Stringify an integer in a particular base. Defaults to decimal (base 10)."
  [int &opt base]
  (default base 10)
  (if-not (<= 2 base (length int-alphabet))
    (error "invalid base"))
  (if (not= int (math/trunc int))
    (error "number is not an integer"))
  (def buf
    (buffer/new
      (+
        (math/ceil (/ (math/log (math/abs int)) (math/log base)))
        (if (< int 0) 1 0))))
  (var int int)
  (def ngtv? (< int 0))
  (if ngtv?
    (set int (- int)))
  (while (not= int 0)
    (def digit (mod int base))
    (buffer/push buf (int-alphabet digit))
    (set int (int/ int base)))
  (if ngtv?
    (buffer/push buf "-"))
  (string/reverse buf))

(defmacro binary-search
  ``Returns the index of `x` in a sorted array or tuple or the index of
  the next item if `x` is not present. This is the correct insert index
  for `x` within `arr`. If a `<?` comparator is given, the search uses
  that to compare elements, otherwise uses `<`.``
  [x arr &opt <?]
  (default <? <)
  (with-syms [start end mid]
    ~(do
       (var ,start 0)
       (var ,end (length ,arr))
       (var ,mid (brshift ,end 1))
       (while (not= ,mid ,end)
         (if (,<? (in ,arr ,mid) ,x)
           (set ,mid (brshift (+ (set ,start ,mid) ,end 1) 1))
           (set ,mid (brshift (+ ,start (set ,end ,mid)) 1))))
       ,mid)))

(defmacro binary-search-by
  ``Returns the index of `x` in an array or tuple which has been sorted
  by a mapping function `f`, or the index of the next item if `x` is not
  present. This is the correct insert index for `x` within `arr`.``
  [x arr f]
  (with-syms [start end mid val]
    ~(do
       (var ,start 0)
       (var ,end (length ,arr))
       (var ,mid (brshift ,end 1))
       (def ,val (,f ,x))
       (while (not= ,mid ,end)
         (if (< (,f (in ,arr ,mid)) ,val)
           (set ,mid (brshift (+ (set ,start ,mid) ,end 1) 1))
           (set ,mid (brshift (+ ,start (set ,end ,mid)) 1))))
       ,mid)))

(defn insert-sorted
  ```
  Insert elements in `arr` such that it remains sorted by the comparator. If
  `arr` is not sorted beforehand, the results are undefined. Returns `arr`.
  ```
  [arr <? & xs]
  (each x xs
    (array/insert arr (binary-search x arr <?) x))
  arr)

(defn insert-sorted-by
  ```
  Insert elements in `arr` such that it remains sorted by the value returned
  when `f` is called with the element, comparing the values with `<`. If `arr` is
  not sorted beforehand, the results are undefined. Returns `arr`.
  ```
  [arr f & xs]
  (each x xs
    (array/insert arr (binary-search-by x arr f) x))
  arr)

(defn merge-sorted
  ``Merges two sorted arrays so that the result remains sorted, using an optional comparator.
  If no comparator is given, `<` is used.``
  [a b &opt <?]
  (default <? <)
  (def len (+ (length a) (length b)))
  (def res (array/new-filled len))
  (var [i j] [0 0])
  (forv k 0 len
    (def [u v] [(get a i) (get b j)])
    (cond
      (= nil u) (do (put res k v) (++ j))
      (= nil v) (do (put res k u) (++ i))
      (<? u v) (do (put res k u) (++ i))
      (do (put res k v) (++ j))))
  res)

(defn merge-sorted-by
  ``Merges two sorted arrays so that result remains sorted when `f` is called on each element,
  comparing the values with `<`.``
  [a b f]
  (merge-sorted a b |(< (f $0) (f $1))))

(def- id-bytes 10)
(defn make-id
  ```
  Create a random, printable keyword id with 10 bytes of entropy
  with an optional prefix.
  ```
  [&opt prefix]
  (default prefix "")
  (def bytes (string/bytes (os/cryptorand id-bytes)))
  (keyword
    prefix
    (string/format
      (comptime (string/repeat "%.2X" id-bytes))
      ;bytes)))
