###
### Re-implementation of mendoza markup. Designed to work with htmlgen.
###

(import ./htmlgen)

(defdyn *front-matter* "Dynamic binding to front matter of markup after parsing, compilation, and evaluation completes.")
(defdyn *markup-dom* "The htmlgen source that can be used to generate a document with htmlgen/html.")

(defn- capture-front
  "Capture the front matter"
  [chunk]
  (def p (parser/new))
  (parser/consume p chunk)
  (parser/eof p)
  (def ret @[])
  (while (parser/has-more p) (array/push ret (parser/produce p)))
  ret)

(defn- capture-value
  "Parse a janet value capture in a pattern. At this point, we
  should already know that the source is valid."
  [chunk]
  (def p (parser/new))
  (parser/consume p chunk)
  (parser/eof p)
  (parser/produce p))

(defn- capture-node
  "Capture a node in the grammar."
  [line col name & params]
  (tuple/setmap ~(,(symbol name) ,;params) line col))

(def- symchars
  "peg for valid symbol characters."
  '(+ (range "09" "AZ" "az" "\x80\xFF") (set "!$%&*+-./:<?=>@^_")))

(def- value-grammar
  "Grammar to get the source for a valid janet value. As it
  doesn't parse the source, it can be a bit shorter and simpler."
  ~{:ws (set " \v\t\r\f\n\0")
    :readermac (set "';~,|")
    :symchars ,symchars
    :token (some :symchars)
    :hex (range "09" "af" "AF")
    :escape (* "\\" (+ (set "ntrvzf0e\"\\") (* "x" :hex :hex)))
    :comment (* "#" (any (if-not (+ "\n" -1) 1)))
    :symbol (if-not (range "09") :token)
    :bytes (* (? "@") "\"" (any (+ :escape (if-not "\"" 1))) "\"")
    :long-bytes {:delim (some "`")
                 :open (capture :delim :n)
                 :close (cmt (* (not (> -1 "`")) (-> :n) ':delim) ,=)
                 :main (drop (* (? "@") :open (any (if-not :close 1)) :close))}
    :number (drop (cmt ':token ,scan-number))
    :raw-value (+ :comment :number :bytes :long-bytes
                  :ptuple :btuple :struct :symbol)
    :value (* (any (+ :ws :readermac)) :raw-value)
    :root (any :value)
    :root2 (any (* :value :value))
    :ptuple (* (? "@") "(" :root (any :ws) ")")
    :btuple (* (? "@") "[" :root (any :ws) "]")
    :struct (* (? "@") "{" :root2 (any :ws) "}")
    :main (/ ':value ,capture-value)})

# Some capture functions to make markup a bit
# more like markdown. This is useful in the common
# case.

(defn- capp [& content]
  (unless (empty? content)
    [tuple :p ;content]))

(defn- get-id-for-content
  "Map the content of a tag to an ID to use in the generated HTML."
  [id-tbl content]
  (when-let [cand (first content)]
    (when (string? cand)
      (let [norm-name (string/replace-all " " "-" (string/trim cand))]
        (if-let [cnt (get id-tbl norm-name)]
          (do
            (put id-tbl norm-name (inc cnt))
            (string norm-name "-" cnt))
          (do
            (put id-tbl norm-name 0)
            norm-name))))))

(defn- caph [n id-tbl & content]
  [tuple (keyword "h" (length n))
   {"id" (get-id-for-content id-tbl content)}
   ;content])

(defn- capture-li
  [& items]
  [tuple :li ;items])

(defn- capture-ulist
  [& items]
  [tuple :ul ;items])

(defn- capture-olist
  [& items]
  [tuple :ol ;items])

(def- markup-grammar
  "Grammar for markup -> document AST parser."
  ~{# basic character classes
    :wsnl (set " \t\r\v\f\n")
    :ws (set " \t\r\v\f")
    :nl (* (? "\r") "\n")
    :nlcap (* (? "\r") '"\n")
    :nl? (* (? "\r") (? "\n"))
    :nlcap? (* (? "\r") (? '"\n"))
    :$ (> 0 (+ "\n" "\r\n"))
    :^ (> -1 (+ -1 "\n"))

    # A span of markup that is not line delimited (most markup)
    :leaf (% (some (+ (* "\\" '1) (if-not (set "@}") '1))))

    # A span or markup that is line delimited (headers, etc). @ expressions
    # can still cross line boundaries.
    :char-line (+ (* "\\" '1) (if-not (set "@}\n\r") '1))
    :leaf-line (% (some :char-line))
    :root-line (some (+ :node :leaf-line))
    :root-lines (some (+ (* :node :nlcap?) (* :leaf-line :nlcap?)))

    # An @ expression (a node)
    :node {:paren-params (* "(" (any :wsnl) (any (* :janet-value (any :wsnl))) ")")
           :string-param (* (> 0 "\"") :janet-value)
           :longstring-param (* (> 0 "`") :janet-value)
           :curly-params (* "{" (/ (any :root) ,array) "}")
           :bracket-params (* "[" '(any (if-not "]" 1)) "]")
           :params (any (* (any :wsnl) (+ :bracket-params :curly-params :paren-params :string-param :longstring-param)))
           :name '(if-not (range "09") (some ,symchars))
           :main (/ (* (line) (column) "@" (+ :name (constant "identity")) :params) ,capture-node)}

    # Front matter
    :front (/ '(any (if-not "---" 1)) ,capture-front)

    :janet-value ,value-grammar

    # Headers (only at top level)
    :header (/ (* '(between 1 6 "#") (any :ws) (argument 0) :root-line) ,caph)

    # Lists
    :li-content (+ (* (some :ws) :root-line) :$)
    :ulist-el (/ (* :^ (any :ws) "-" :li-content) ,capture-li)
    :ulist (/ (some (* :ulist-el :nl?)) ,capture-ulist)
    :olist-el (/ (* :^ (any :ws) :d+ "." :li-content) ,capture-li)
    :olist-el1 (/ (* :^ (any :ws) (+ "1." "0.") :li-content) ,capture-li)
    :olist (/ (* :olist-el1 :nl? (any (* :olist-el :nl?))) ,capture-olist)

    # Node-level markup - same as top level minus trailing } and no paragraphs
    :root (+ :ulist
             :olist
             :nlcap
             '(some :ws)
             :node
             :header
             (/ :root-lines ,array))

    # Top-level markup
    :top-level (any
                 (+ :ulist
                    :olist
                    :nlcap
                    '(some :ws)
                    (* :node (any :wsnl))
                    :header
                    (/ :root-lines ,capp)
                    "}"))

    # Main rule: Front matter -> Top level nodes and markup
    :main (*
            (+ :front (error "bad front-matter"))
            "---"
            :top-level
            (+ -1 (error)))})

(def- markup-peg
  "A peg that converts markdown to html."
  (peg/compile markup-grammar))

###
### Base-env
###

(each tag ["ul" "ol" "li" "p" "em" "strong" "u" "pre" "sub" "sup" "tr" "td" "th" "div"]
  (setdyn (symbol tag)
          @{:doc (string "Make a " tag " element")
            :value (fn [content]
                     [(keyword tag) {} content])}))

(defn tag
  "Wrap some content in an html tag. If you need attributes or other properties,
  you may want to use raw HTML via the html function."
  [name content]
  [name content])

(defn hr
  "Add a horizontal rule"
  []
  [:hr])

(defn bigger
  "Make span element with bigger font"
  [content]
  [:span {"style" "font-size:1.61803398875em;"} content])

(defn smaller
  "Make span element with smaller font"
  [content]
  [:span {"style" "font-size:0.61803398875em;"} content])

(defn code
  "Make code element with class mendoza-code"
  [content]
  [:code {"class" "mendoza-code"} content])

(defn anchor
  "Create an in-page anchor for a local link."
  [name & content]
  [:a {"name" name} ;content])

(defn link
  "Create an anchor link"
  [url &opt content]
  [:a {"href" url} (or content url)])

(defn section
  "Create a section. Usually used to embed different parts of the content
  document into different parts of the main page."
  [name content]
  [:section {"name" name} content])

(defn blockquote
  "Make a block quote element"
  [content]
  [:blockquote content])

(defn image
  "Make an image element"
  [src alt]
  [:img {:src src :alt alt}])

(defn center
  "Center some content"
  [content]
  [:div {"class" "mendoza-center"} content])

(defn html
  "Embed some raw html"
  [source]
  (htmlgen/raw source))

(defn codeblock
  "Inline code or codeblock"
  [lang &opt source]
  (def source2 (or source lang))
  (def lang2 (if source lang nil))
  [:pre {"class" "mendoza-codeblock"}
   # wrap source2 in array so it can be mutated external by a highlighter
   [:code {"data-language" lang2} @[source2]]])

###
### Markup Entrypoint
###

(def- base-env (curenv))

(defn markup
  "Parse mendoza markup and evaluate it returning an htmlgen document tree."
  [source &opt env where]
  (default where "<anonymous>")
  (default env (let [e (make-env base-env)] (put e :current-file where) e))
  (def matches (peg/match markup-peg source 0 @{}))
  (unless matches (error "bad markdown"))

  # Evaluate markup forms inside the given environment, with error handling
  (defn- eval1 [ast]
    (def thunk (compile ast env where))
    (unless (function? thunk)
      (error (string where ":" (thunk :line) ":" (thunk :column) ": " (thunk :error))))
    (def f (fiber/new thunk :))
    (fiber/setenv f env)
    (resume f))

  # Eval front-matter
  (var front-matter nil)
  (each form (in matches 0) (set front-matter (eval1 form)))
  (assert (dictionary? front-matter) "front-matter must be janet table or struct")
  (put env *front-matter* front-matter)

  # Eval body of markup
  (def contents @[])
  (eval1 ~(do ,;(map |~(,array/push ',contents ,$) (slice matches 1))))
  (def markup ((get front-matter :post-process identity) contents))
  (put env *markup-dom* markup)
  env)

###
### Loader
###

(defn mdz-loader
  "Loader for the mdz format"
  [path &]
  (def env (markup (slurp path) nil path))
  (put env 'markup-dom @{:value (get env *markup-dom*)})
  (put env 'front-matter @{:value (get env *front-matter*)})
  env)

(defn add-loader
  "Allow importing and requiring markup as a module"
  []
  (put module/loaders :spork-mendoza-markup mdz-loader)
  (module/add-paths ".mdz" :spork-mendoza-markup))
