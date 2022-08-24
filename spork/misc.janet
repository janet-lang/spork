###
### misc.janet
###
### One-off functions that don't need their own module.
###

(defn dedent
  "Remove indentation after concatenating the arguments. Works by removing
  leading whitespace, and then removing that same pattern of whitepsace after
  new lines."
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
    (let [y (string/replace-all (string "\n" indent) "\n" (string/replace indent "" x))]
      # Remove trailing newline to mimic long string newline omission.
      (if (= (chr "\n") (last y))
        (slice y 0 -2)
        y))
    x))

(defmacro set*
  ```Parallel `set` function.  Takes a list of targets and
  expressions, evaluates all the expressions, and then
  assigns them to the targets.  Each target can be a variable
  or a 2-tuple, just like in the normal `set` special form.```
  [tgts exprs]
  (when (not= (length tgts) (length exprs)) (error "Expected tgts and exprs to have same length"))
  (def code @['do])
  (def syms @[])
  (loop [e :in exprs]
    (def sym (gensym))
    (array/push syms sym)
    (array/push code (tuple 'def sym e)))
  (loop [[i t] :pairs tgts]
    (array/push code (tuple 'set t (in syms i))))
  (tuple ;code))

(defn print-table
  "Iterate through the rows of a data structure and print a table in a human readable way,
  with padding and heading information. Can optionally provide a function use to print a row, as well
  as optionally select column keys for each row. Lastly, a `header-mapping` dictionary can be provided
  that changes the printed header names my mapping column keys to the desired header name. If no mapping is
  found, then the column key will be used as the header name. Returns nil."
  [data &opt columns header-mapping]
  (var colkeys columns)
  (def column-widths @{})
  (def processed @[])
  (default header-mapping {})

  # Preprocess rows
  (each row data
    (unless colkeys
      (set colkeys (sorted (keys row))))
    (def newrow @[])
    (each key colkeys
      (def item (string (in row key)))
      (set (column-widths key) (max (length item) (get column-widths key 0)))
      (array/push newrow item))
    (array/push processed newrow))

  # Apply width of header names
  (each key colkeys
    (def header (get header-mapping key key))
    (set (column-widths key) (max (length header) (get column-widths key 0))))

  # Generate format string
  (var bar-width 0)
  (def fbuf @"")
  (each key colkeys
    (def width (+ 2 (get column-widths key 6)))
    (+= bar-width width)
    (buffer/push fbuf "%" (string width) "s"))
  (def format-string (string fbuf))

  # Print header
  (each key colkeys
    (def header (get header-mapping key key))
    (def str (string header (string/repeat " " (- (column-widths key) (length header) -2))))
    (prin str))
  (print)
  (print (string/repeat "═" bar-width))

  # Finally body
  (each row processed
    (printf format-string ;row)))

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
  "Do a depth first, pre-order traversal over a data structure.
  Also allow for callbacks before and after visiting the children
  of a node. Also allow for a custom `get-children` function to
  change traversal as needed. Will detect cycles if an empty table
  is passed as the `seen` parameter, which is used to cached values
  that have been visited."
  [data visit-leaf &opt node-before node-after get-children seen]
  (default get-children default-traversal-predicate)
  (when seen
    (if (in seen data) (break))
    (put seen data true))
  (if-let [node (get-children data)]
    (do
      (when node-before (node-before node))
      (each child node (dfs child visit-leaf node-before node-after get-children seen))
      (when node-after (node-after node)))
    (visit-leaf data)))

(defn randomize-array
  "Randomizes array using the fisher-yates shuffle, takes an optional random number generator."
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
