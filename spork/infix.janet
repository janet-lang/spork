###
### infix.janet - A macro for infix syntax in Janet. Useful for math.
###
### Examples:
###
###     ($$ a + b ** 2)            --->   (+ a (math/pow b 2))
###     ($$ (a + b) ** 2)          --->   (math/pow (+ a b) 2)
###     ($$ y[2] + y[3])           --->   (+ (in y 2) (in y 3))
###     ($$ a > b and ,(good? z))  --->   (and (> a b) (good? z))
###
### Syntax is as follows:
###
###   Binary operators <<, >>, >>>, =, !=, <, <=, >, >=, &, ^, bor, band, and, or,
###   +, -, *, /, %, ** are supported. Operator precedence is in the
###   `precedence table below (higher means more tightly binding). All
###   operators are left associative except ** (math/pow), which is right
###   associative.
###
###   Unary prefix operators !, -, bnot, not, ++, -- are supported.
###   No unary postfix operators are supported.
###
###   Square brackets can be used for indexing.
###
###   Normal parentheses are used for making subgroups
###
###   You can "escape" infix syntax use a quote or unquote (comma)
###

(def- precedence
  {'>> 9
   '<< 9
   '>>> 9
   '= 8
   '!= 8
   'not= 8
   '< 8
   '<= 8
   '>= 8
   '> 8
   '& 7
   '^ 6
   'bor 5
   'band 5
   'and 4
   'or 3
   '+ 10
   '- 10
   '* 20
   '/ 20
   '% 20
   '** 30
   '! 40
   'not 40
   'bnot 40
   '++ 40
   '-- 40})

(def- right-associative
  {'** true})

(def- unary
  {'! true '- true 'bnot true 'not true '++ true '-- true})

(def- replacements
  {'** math/pow
   '>> brshift
   '<< blshift
   '>>> brushift
   '^ bxor
   '! not
   '!= not=
   '& band})

(defn- tup? [x] (and (tuple? x) (= (tuple/type x) :parens)))
(defn- brak? [x] (and (tuple? x) (= (tuple/type x) :brackets)))

(defn- parse-tokens
  [raw-tokens]
  # Allow breaking out of infix syntax with ' or ,
  (when (= 'quote (first raw-tokens))
    (break raw-tokens))
  (when (= 'unquote (first raw-tokens))
    (break (get raw-tokens 1)))
  (def tokens
    (keep-syntax
      raw-tokens
      (map |(if (tup? $) (parse-tokens $) $) raw-tokens)))
  (var i -1)
  (defn eat [] (get tokens (++ i)))
  (defn uneat [] (-- i))
  (defn parse-expression
    [lhs min-prec]
    (when (get unary lhs)
      (break (parse-expression
               (keep-syntax raw-tokens [(get replacements lhs lhs)
                                        (parse-expression (eat) (get precedence lhs 0))])
               min-prec)))
    (def op (eat))
    (def prec (get precedence op 0))
    (cond
      (nil? op) lhs # done

      (brak? op) # array subscripting (highest precedence)
      (let [index (parse-tokens op)]
        (parse-expression [in lhs index] min-prec))

      # Function application for math/sin, etc.
      (tup? op)
      (parse-expression [lhs op] min-prec)

      (zero? prec) (errorf "expected binary operator, got %p" op)

      ((if (get right-associative op) >= >) prec min-prec) # climb precendence
      (let [next-token (eat)
            rhs (parse-expression next-token prec)
            real-op (get replacements op op)]
        (parse-expression (keep-syntax raw-tokens [real-op lhs rhs]) min-prec))

      :else # lower precedence
      (do (uneat) lhs)))
  (def ret (parse-expression (eat) 0))
  (when (= nil ret)
    (errorf "expected non-empty expression, got %p" raw-tokens))
  ret)

(defmacro $$
  "Use infix syntax for writing expressions in a more familiar manner. Useful for writing mathematic expressions."
  [& body]
  (def res (parse-tokens body))
  res)
