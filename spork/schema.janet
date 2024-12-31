###
### schema.janet
### Copyright © Calvin Rose 2021
###
### Simple schema validation library. Specify structure declaratively, and get
### functions that will check that structure and either raise an error or return a boolean.
### While reasonably general, use is intended for data such as one would find
### in configuration files or network protocols.
###
### Currently does not support more advanced features such as:
###   - Recursive schemas
###   - Full error reporting (only a single error is reported)
###   - PEG style grammars (used to enable recursion in PEGs)
###   - Unification (such as in the `match` macro)
###   - Parsing/data extraction
###
### Syntax:
###  - :keyword - match any value of that type
###  - Tuples are used to match various combinators:
###    - (any) - match any one value
###    - (enum & options) - match any of the option values
###    - (or & schemas) - similar to enum, but each option is considered a schema.
###    - (and & schemas) - Only matches if all clauses match
###    - (values schema) - Matches only if schema matches all values in a data structure.
###    - (keys schema) - Matches only if schema matches all keys in a data structure.
###    - (props & k v) - Takes a sequence of keys and values (alternating in order). Only matches
###      the data if, for a key, the corresponding schema `v` matches.
###    - (length l) - Only match if the data has a length of l. Uses of the length combinator should assert
###      the data type before doing a length check.
###    - (length min max) - Only match lengths between min and max inclusive
###    - (peg pattern) - Matches only if the peg matches
###    - (not pattern) - Only matches if pattern does not match
###    - (pred predicate) - Use a predicate function (function of 1 argument) to check if the data is valid.
###  - anything else - match that value literally
###

(defn- compile-schema
  "Given a schema definition, compile to a predicate function to validate the schema."
  [x pattern short-circuit break-val]

  # Fragment to check for early exit
  (def breaker (if (and short-circuit (not= nil break-val)) ~(if ,short-circuit (break ,break-val)) nil))

  (defn invalid
    "Create code fragment to mark ,x as invalid."
    [msg &opt modifier]
    (if short-circuit
      ~(set ,short-circuit true)
      ~(,errorf ,(string "failed clause %p, " msg) ',pattern ,(if modifier ~(,modifier ,x) x))))

  (case (type pattern)

    # Simple types
    :keyword
    ~(if (,not= ,pattern (,type ,x))
       ,(invalid (string "expected value of type " pattern ", got %v")))

    # Various combinators
    :tuple
    (match pattern

      # Match anything
      [(a (= a 'any))]
      nil

      # Simple value matching
      [(a (= a 'enum))]
      (let [ys (slice pattern 1)]
        ~(if (or ,;(seq [y :in ys] ~(,= ,y ,x)))
           nil
           ,(invalid (string/format "expected one of %p, got %%v" ys))))

      # Union
      [(a (= a 'or))]
      (let [rest (slice pattern 1)
            still-looking (gensym)
            ss2 (gensym)]
        ~(do
           (var ,still-looking true)
           ,;(seq [pat :in rest]
               ~(if ,still-looking
                  (do
                    (var ,ss2 false)
                    ,(compile-schema x pat ss2 nil)
                    (if ,ss2 nil (set ,still-looking false)))))
           (if ,still-looking ,(invalid "choice failed: %v"))))

      # Intersection
      [(a (= a 'and))]
      (let [rest (slice pattern 1)
            part (fn [y] (compile-schema x y short-circuit break-val))]
        ~(do ,;(interpose breaker (map part rest))))

      # Assert for all values
      [(a (= a 'values)) subpat]
      (with-syms [iterator]
        ~(each ,iterator ,x
           ,breaker
           ,(compile-schema iterator subpat short-circuit break-val)))

      # Assert for all keys
      [(a (= a 'keys)) subpat]
      (with-syms [iterator]
        ~(eachk ,iterator ,x
           ,breaker
           ,(compile-schema iterator subpat short-circuit break-val)))

      # Assert structure for tables and structs
      [(a (= a 'props))]
      (let [rest (slice pattern 1)
            ps (partition 2 rest)]
        (assert (even? (length rest)) "expected key-values, got odd number of elements")
        ~(do
           ,;(seq [[k v] :in ps]
               (def tester (gensym))
               ~(do ,breaker (def ,tester (,get ,x ,k)) ,(compile-schema tester v short-circuit break-val)))))

      # Assert length
      [(a (= a 'length)) minl maxl]
      ~(if (,not (<= ,minl (,length ,x) ,maxl))
         ,(invalid
            (string "expected length to be in range " minl " to " maxl ", got %v instead")
            length))

      [(a (= a 'length)) l]
      ~(if (,not= ,l (,length ,x))
         ,(invalid
            (string "expected length to be " l ", got %v instead")
            length))

      # PEG test
      [(a (= a 'peg)) peg]
      (let [compiled-peg (if (abstract? peg) peg (peg/compile peg))]
        ~(if (bytes? ,x)
           (if (peg/match ,compiled-peg ,x) nil
             ,(invalid
                (string "peg match failed against " peg " for value %v")))
           ,(invalid "expected bytes, got %v")))

      # Not
      [(a (= a 'not)) rule]
      (with-syms [ss]
        ~(do
           (var ,ss false)
           ,(compile-schema x rule ss false) # TODO - early exits here
           (if ,ss nil ,(invalid "failed not clause: %v"))))

      # Arbitrary predicates
      [(a (= a 'pred)) pred]
      ~(if (,pred ,x) nil
         ,(invalid
            (string/format "predicate %v failed for value %%v" pred)))

      # Unknown
      _ (errorf "unexpected schema: %p" pattern))

    # default
    ~(if (,not= ,pattern ,x)
       ,(invalid (string "expected " pattern ", got %v")))))

#
# Constructors
#

(defmacro validator
  "Make a validation function of one argument.
  A validation function will throw an error on validation failure, otherwise, it will return the argument."
  [pattern]
  (with-syms [arg]
    (def body (compile-schema arg pattern nil nil))
    # (printf "%.99M" body)
    ~(fn validate [,arg] ,body ,arg)))

(defn make-validator
  "Generate a function that can be used to validate a data structure. This is the function
  form of `validator`."
  [schema]
  (compile (apply validator [schema])))

(defmacro predicate
  "Make a validation predicate given a certain schema."
  [pattern]
  (with-syms [arg flag]
    (def body (compile-schema arg pattern flag false))
    # (printf "%.99M" body)
    ~(fn check [,arg] (var ,flag false) ,body (,not ,flag))))

(defn make-predicate
  "Generate a function that can be used to validate a data structure. This is the function
  form of `predicate`."
  [schema]
  (compile (apply predicate [schema])))
