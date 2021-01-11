###
### regex.janet
###
### A module for compiling a subset of regexes to Janet PEGs.
### All regex are considered to be anchored, and performance is
### is not going to be competitive with a native regex engine.
###
###
### Supported regex features:
###   - single bytes
###   - escape characters
###   - +, *, ?, .
###   - Repetitions, e.g. abc{1}, abc{1,3}. Repetitions are eagerly evaluated.
###   - Ranges, e.g. [A-Za-z]
###   - Character classes, inverted character classes, e.g. [abc], [^abc]
###   - Alteration (choice), except alteration is ordered, as in pegs - e.g a|b|c
###   - Captures using parentheses, .e.g (abc)
###   - Non-capture groups, e.v. (?:abc)
###
###
### Features found in other regex may never be added - for more complex usage,
### use Janet's native PEG library.
###

(defn- postfix-modify
  "Apply regex postfix operators to a pattern."
  [cc suffix &opt suf2]
  (case suffix 
    "?" ['? cc]
    "*" ['any cc]
    "+" ['some cc]
    (if (empty? suffix)
      cc
      (if suf2
        ['between (scan-number suffix) (scan-number suf2) cc]
        ['repeat (scan-number suffix) cc]))))

(defn- make-sequence
  "Combine a series of patterns into a sequence combinator, but
  merge string literals together."
  [& ccs]
  (let [res @['*]
        buf @""]
    (each cc ccs
      (if (string? cc)
        (buffer/push-string buf cc)
        (do
          (unless (empty? buf)
            (array/push res (string buf)))
          (array/push res cc)
          (buffer/clear buf))))
    (unless (empty? buf) (array/push res (string buf)))
    (if (= 2 (length res)) (in res 1) (tuple/slice res))))

(defn- make-choice
  "Combine multiple patterns with the choice combinator, or
  return a the first argument if there is only one argument.
  Will also reduce multiple choices into a single choice operator."
  [l &opt r]
  (if r
    (if (and (tuple? r) (= 'choice (first r)))
      ['choice l ;(tuple/slice r 1)]
      ['choice l r])
    l))

(def peg
  "Peg used to generate peg source code from a regular expression string."
  (peg/compile
    ~{
      # Custom character classes (bracketed characters)
      # Compiled to a single (range ...) peg combinator
      :hex (range "09" "af" "AF")
      :escapedchar (+ (/ `\n` "\n")
                      (/ `\t` "\t")
                      (/ `\e` "\e")
                      (/ `\v` "\v")
                      (/ `\r` "\r")
                      (/ `\f` "\f")
                      (/ `\0` "\0")
                      (/ `\z` "\z")
                      (/ (* `\x` '(* :hex :hex)) ,|(string/from-bytes (scan-number (string "0x" $))))
                      (* `\` '1))
      :namedclass1 (+ (/ `\s` "  \t\n")
                      (/ `\d` "09")
                      (/ `\a` "AZaz")
                      (/ `\w` "AZaz09")
                      (/ `\S` "\0\x08\x0e\x1f\x21\xff")
                      (/ `\D` "\0\x2f\x3a\xff")
                      (/ `\A` "\0\x40\x5b\x60\x7b\xff")
                      (/ `\W` "\0\x2f\x3a\x40\x5b\x60\x7b\xff"))
      :singbyte (+ :escapedchar (if-not (set "-]") '1))
      :singchar (/ :singbyte ,|(string $ $))
      :charspan (* :singbyte "-" :singbyte)
      :make-range (/ (accumulate (any (+ :namedclass1 :charspan :singchar)))
                     ,|['range ;(partition 2 $)])
      :brack (* "[" :make-range "]")
      :invbrack (/ (* "[^" :make-range "]") ,|['if-not $ 1])

      # Other single characters
      :escapedchar2 (+ (/ `\s` (set "\n\t\r\v\f "))
                       (/ `\d` (range "09"))
                       (/ `\a` (range "AZ" "az"))
                       (/ `\w` (range "AZ" "az" "09"))
                       (/ `\S` (range "\0\x08" "\x0e\x1f" "\x21\xff"))
                       (/ `\D` (range "\0\x2f" "\x3a\xff"))
                       (/ `\A` (range "\0\x40" "\x5b\x60" "\x7b\xff"))
                       (/ `\W` (range "\0\x2f" "\x3a\x40" "\x5b\x60" "\x7b\xff"))
                       :escapedchar)
      :normalchars (if-not (set `[.+*?()|`) '1)
      :dot (/ "." 1)
      :cc (+ :dot :invbrack :brack :escapedchar2 :normalchars)

      # Postfix modifier
      :postfix (+ '"?" '"*" '"+" (* "{" ':d+ (? (* "," ':d+)) "}") '"")

      # Character modifiers
      :cc-with-mod (/ (* :cc :postfix) ,postfix-modify)

      # Single alteration is a sequence of character classes.
      :span (/ (some :cc-with-mod) ,make-sequence)

      # Captures and groupings
      :grouping (* "(?:" :node ")")
      :capture (/ (* "(" :node ")") ,|['capture $])
      :node1 (/ (* (+ :grouping :capture :span) :postfix) ,postfix-modify)
      :node-span (/ (some :node1) ,make-sequence)
      :node (/ (* :node-span (? (* "|" :node))) ,make-choice)

      :main (* :node (+ -1 (error "")))}))

(defn source
  "Compile a subset of regex to PEG source code."
  [pattern]
  (def [res] (peg/match peg pattern))
  res)

(defn compile
  "Compile a subset of regex to a PEG if pattern is a string.
  If pattern is a PEG, will return the PEG as is."
  [pattern]
  (if (string? pattern)
    (peg/compile (source pattern))
    pattern))

(defn match
  "Similar to peg/match, but for regexes."
  [reg text &opt start]
  (peg/match (compile reg) text (or start 0)))

(defn find
  "Similar to peg/find, but for regexes."
  [reg text &opt start]
  (peg/find (compile reg) text (or start 0)))

(defn find-all
  "Similar to peg/find-all, but for regexes."
  [reg text &opt start]
  (peg/find-all (compile reg) text (or start 0)))

(defn replace
  "Similar to peg/replace, but for regexes."
  [reg rep text &opt start]
  (peg/replace (compile reg) rep text (or start 0)))

(defn replace-all
  "Similar to peg/replace-all, but for regexes."
  [reg rep text &opt start]
  (peg/replace-all (compile reg) rep text (or start 0)))
