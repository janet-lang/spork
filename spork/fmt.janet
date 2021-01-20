###
### fmt.janet
###
### Janet code formatter.
###

(defn- pnode
  "Make a capture function for a node."
  [tag]
  (fn [x] [tag x]))

(def- parse-peg
  "Peg to parse Janet with extra information, namely comments."
  (peg/compile
    ~{:ws (+ (set " \t\r\f\0\v") '"\n")
      :readermac (/ '(set "';~,|") ,(pnode :readermac))
      :symchars (+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_"))
      :token (some :symchars)
      :hex (range "09" "af" "AF")
      :escape (* "\\" (+ (set "ntrzfev0\"\\")
                         (* "x" :hex :hex)
                         (* "u" :hex :hex :hex :hex)
                         (* "U" :hex :hex :hex :hex :hex :hex)
                         (error (constant "bad hex escape"))))
      :comment (/ (* "#" '(any (if-not (+ "\n" -1) 1)) (+ "\n" -1)) ,(pnode :comment))
      :spacing (any (* (any :ws) (? :comment)))
      :span (/ ':token ,(pnode :span))
      :bytes '(* "\"" (any (+ :escape (if-not "\"" 1))) "\"")
      :string (/ :bytes ,(pnode :string))
      :buffer (/ (* "@" :bytes) ,(pnode :buffer))
      :long-bytes '{:delim (some "`")
                    :open (capture :delim :n)
                    :close (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=)
                    :main (drop (* :open (any (if-not :close 1)) :close))}
      :long-string (/ :long-bytes ,(pnode :string))
      :long-buffer (/ (* "@" :long-bytes) ,(pnode :buffer))
      :raw-value (+ :string :buffer :long-string :long-buffer
                    :parray :barray :ptuple :btuple :struct :dict :span)
      :value (* :spacing (any (+ :ws :readermac)) :raw-value :spacing)
      :root (any :value)
      :root2 (any (* :value :value))
      :ptuple (/ (group (* "(" :root (+ ")" (error)))) ,(pnode :ptuple))
      :btuple (/ (group (* "[" :root (+ "]" (error)))) ,(pnode :btuple))
      :struct (/ (group (* "{" :root2 (+ "}" (error)))) ,(pnode :struct))
      :parray (/ (group (* "@(" :root (+ ")" (error)))) ,(pnode :array))
      :barray (/ (group (* "@[" :root (+ "]" (error)))) ,(pnode :array))
      :dict (/ (group (* "@{" :root2 (+ "}" (error)))) ,(pnode :table))
      :main (* :root (+ -1 (error)))}))

(defn- make-tree
  "Turn a string of source code into a tree that will be printed"
  [source]
  [:top (peg/match parse-peg source)])

(defn- remove-extra-newlines
  "Remove leading and trailing newlines. Also remove
   some some extra consecutive newlines."
  [node]
  (match node
    [tag (xs (array? xs))]
    (do
      (while (= "\n" (array/peek xs)) (array/pop xs)) # remove trailing newlines
      (when-let [index (find-index |(not= "\n" $) xs)]
        (array/remove xs 0 index)) # remove leading newlines
      # remove too many consecutive newlines
      (def max-consec (if (= tag :top) 3 2))
      (var i 0)
      (var consec-count 0)
      (while (< i (length xs))
        (if (= (in xs i) "\n")
          (if (= consec-count max-consec) (array/remove xs i) (do (++ i) (++ consec-count)))
          (do (set consec-count 0) (++ i))))
      node)
    node))

(def- indent-2-forms
  "A list of forms that are control forms and should be indented two spaces."
  (invert ["fn" "match" "with" "with-dyns" "def" "def-" "var" "var-" "defn" "defn-"
           "varfn" "defmacro" "defmacro-" "defer" "edefer" "loop" "seq" "generate" "coro"
           "for" "each" "eachp" "eachk" "case" "cond" "do" "defglobal" "varglobal"
           "if" "when" "when-let" "when-with" "while" "with-syms" "with-vars"
           "if-let" "if-not" "if-with" "let" "short-fn" "try" "unless" "default" "forever"
           "repeat" "eachy" "forv"]))

(def- indent-2-peg
  "Peg to use to fuzzy match certain forms."
  (peg/compile ~(+ "with-" "def" "if-" "when-")))

(defn- check-indent-2
  "Check if a tuple needs a 2 space indent or not"
  [items]
  (if-let [[tag body] (get items 0)]
    (cond
      (= "\n" (get items 1)) true
      (not= tag :span) nil
      (in indent-2-forms body) true
      (peg/match indent-2-peg body) true)))

(defn- fmt
  "Emit formatted."
  [tree]

  (var col 0)
  (def ident-stack @[])
  (var ident "")
  (def white @"")

  (defn emit [& xs] (each x xs (+= col (length x)) (prin x)))
  (defn indent [&opt delta]
    (array/push ident-stack ident)
    (set ident (string/repeat " " (+ col (or delta 0)))))
  (defn dedent [] (set ident (array/pop ident-stack)))
  (defn flushwhite [] (emit white) (buffer/clear white))
  (defn dropwhite [] (buffer/clear white))
  (defn addwhite [] (buffer/push-string white " "))
  (defn newline [] (dropwhite) (print) (buffer/push-string white ident) (set col 0))

  # Mutual recursion
  (var fmt-1-recur nil)

  (defn emit-body
    [open xs close &opt delta]
    (emit open)
    (indent delta)
    (each x xs (fmt-1-recur x))
    (dropwhite)
    (dedent)
    (emit close)
    (addwhite))

  (defn emit-funcall
    [xs]
    (emit "(")
    (def len (length xs))
    (when (pos? len) 
      (fmt-1-recur (xs 0))
      (indent 1)
      (for i 1 len (fmt-1-recur (xs i)))
      (dropwhite)
      (dedent))
    (emit ")")
    (addwhite))

  (defn emit-string
    [x]
    (def parts (interpose "\n" (string/split "\n" x)))
    (each p parts (if (= p "\n") (do (newline) (dropwhite)) (emit p))))

  (defn fmt-1
    [node]
    (remove-extra-newlines node)
    (unless (= node "\n") (flushwhite))
    (match node
      "\n" (newline)
      [:comment x] (do (emit "#" x) (newline))
      [:readermac x] (emit x)
      [:span x] (do (emit x) (addwhite))
      [:string x] (do (emit-string x) (addwhite))
      [:buffer x] (do (emit "@") (emit-string x) (addwhite))
      [:array xs] (emit-body "@[" xs "]")
      [:btuple xs] (emit-body "[" xs "]")
      [:ptuple xs] (if (check-indent-2 xs)
                     (emit-body "(" xs ")" 1)
                     (emit-funcall xs))
      [:struct xs] (emit-body "{" xs "}")
      [:table xs] (emit-body "@{" xs "}")
      [:top xs] (emit-body "" xs "")))

  (set fmt-1-recur fmt-1)
  (fmt-1 tree)
  (newline)
  (flush))

#
# Public API
#

(defn format-print
  "Format a string of source code and print the result."
  [source]
  (-> source make-tree fmt))

(defn format
  "Format a string of source code to a buffer."
  [source]
  (def out @"")
  (with-dyns [:out out]
    (format-print source))
  out)

(defn format-file
  "Format a file"
  [file]
  (def source (slurp file))
  (def out (format source))
  (spit file out))
