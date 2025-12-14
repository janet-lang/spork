###
### cjanet.janet
###
### A DSL that compiles to C. Improved version of jpm/cgen
### that is more amenable to Janet integration, macros,
### and meta-programming.
###
### The semantics of the language are basically the
### same as C so a higher level language (or type system)
### should be built on top of this. This IR emits a very useful
### subset of valid C 99.
###

(import ./cc)
(import ./pm-config)

(def- mangle-peg
  (peg/compile
    ~{:valid (range "az" "AZ" "__" ".." "::")
      :one (+ '"->" (/ "-" "_") ':valid (/ '1 ,|(string "_X" ($ 0))))
      :main (% (* (? "@") :one (any (+ ':d :one))))}))

(def- mangle-strict-peg
  (peg/compile
    ~{:valid (range "az" "AZ" "__")
      :one (+ (/ "-" "_") ':valid (/ '1 ,|(string "_X" ($ 0))))
      :main (% (* (? "@") :one (any (+ ':d :one))))}))

(def- bops
  {'+ '+ '- '- '* '* '/ '/ '% '% '< '<
   '> '> '<= '<= '>= '>= '== '== '= '== '!= '!=
   'not= "!="
   '>> ">>" '<< "<<" '&& "&&" '^ "^"
   'and "&&" 'or "||" 'band "&" 'bor "|" 'bxor "^" 'set "="
   'blshift "<<" 'brshift ">>"})

(def- uops {'bnot "~" 'not "!" 'neg "-" '- "-" '! "!" '++ "++" '-- "--"})

(defn mangle
  ``
  Convert any sequence of bytes to a valid C identifier in a way that is unlikely to collide. The period character
  is left unchanged even though it is not a valid identifier to allow for easy access into structs.
  ``
  [token]
  (first (peg/match mangle-peg token)))

(defn mangle-strict
  ``
  Same as `mangle` but only emit proper C identifiers (no ., :,  or -> allowed).
  ``
  [token]
  (first (peg/match mangle-strict-peg token)))

(def- type-split-peg
  (peg/compile '(* (? (* '(to ":") ":")) '(any 1))))

(defn type-split
  "Extract name and type from a variable. Allow typing variables as both
  (name type) or name:type as a shorthand. If no type is found, default to dflt-type. dflt-type
  itself defaults to '__auto_type"
  [x &opt dflt-type]
  (default dflt-type '__auto_type)
  # This needs to be defined based on c compiler - "auto" for msvc and c23+, and __auto_type for clang and GCC on older standards
  (case (type x)
    :tuple x
    :symbol
    (let [[v t] (assert (peg/match type-split-peg x))]
      [(symbol v) (symbol (or t dflt-type))])
    (errorf "expected symbol or (symbol type) pair, got %j" x)))

(def- type-split-dflt-peg
  (peg/compile '(* '(to ":") ":" '(to "=") "=" '(any 1))))

(defn- type-split-dflt
  "Same as type split, but require a name, type, and default. Supports (name type dflt), as well
  as name:type=dflt."
  [x]
  (case (type x)
    :tuple x
    :symbol
    (let [[v t d] (assert (peg/match type-split-dflt-peg x))]
      [(symbol v) (symbol t) (parse d)])
    (errorf "expected symbol (symbol type dflt) tuple, got %j" x)))

# Macros
# We need to be judicious with macros as they can obscure real C functions. In practice we can get
# around this with mangling magic - add a leading "@" to the symbol and it will not match a macro name

(def- extra-macros @{})

(defn- expand-macro
  "Expand macros given a specific tag"
  [macro-tag form]
  (unless (tuple? form) (break form))
  (def head (first form))
  (unless (symbol? head) (break form))
  (def entry (get extra-macros head (dyn head)))
  (unless (get entry macro-tag) (break form))
  (def expand1 ((get entry :value) ;(drop 1 form)))
  (expand-macro macro-tag expand1))

(defn- register-macro
  [macro-tag name value]
  (put extra-macros (symbol name) {macro-tag true :value value}))

(register-macro :cjanet-block-macro 'when when)
(register-macro :cjanet-block-macro 'if-not if-not)
(register-macro :cjanet-block-macro 'unless unless)
(register-macro :cjanet-block-macro 'let let)
(register-macro :cjanet-statement-macro '+= |~(set ,$0 (+ ,$0 ,$1)))
(register-macro :cjanet-statement-macro '-= |~(set ,$0 (- ,$0 ,$1)))
(register-macro :cjanet-statement-macro '*= |~(set ,$0 (* ,$0 ,$1)))
(register-macro :cjanet-statement-macro '/= |~(set ,$0 (/ ,$0 ,$1)))
(register-macro :cjanet-statement-macro '^= |~(set ,$0 (bxor ,$0 ,$1)))
(register-macro :cjanet-statement-macro 'bxor= |~(set ,$0 (bxor ,$0 ,$1)))
(register-macro :cjanet-statement-macro '&= |~(set ,$0 (band ,$0 ,$1)))
(register-macro :cjanet-statement-macro 'band= |~(set ,$0 (band ,$0 ,$1)))
(register-macro :cjanet-statement-macro 'bor= |~(set ,$0 (bor ,$0 ,$1)))
(register-macro :cjanet-statement-macro '<<= |~(set ,$0 (<< ,$0 ,$1)))
(register-macro :cjanet-statement-macro '>>= |~(set ,$0 (>> ,$0 ,$1)))
(register-macro :cjanet-statement-macro '%= |~(set ,$0 (% ,$0 ,$1)))

###
### Emitting C
###

(def- comment-patch-peg
  (peg/compile '(% (any (+ (/ '"/*" " ") (/ '"*/" " ") '1)))))

(defn emit-comment
  "Emit a multi-line comment string for C"
  [msg]
  (def processed-msg (first (peg/match comment-patch-peg msg)))
  (print "/* " processed-msg " */"))

(defdyn *indent* "current indent buffer")
(defn- indent [] (or (dyn *indent*) (setdyn *indent* @"")))

# Expose indent helpers
(defn emit-indent [] (prin (indent)))
(defn emit-block-start [] (prin "{") (buffer/push (indent) "  ") (print))
(defn emit-block-end [&opt nl] (buffer/popn (indent) 2) (emit-indent) (prin "}") (when nl (print)))

(var- emit-type nil)
(var- emit-expression nil)
(var- emit-statement nil)
(var- emit-block nil)

(defn- emit-struct-union-def
  [which name args defname]
  (when (or (nil? args) (empty? args))
    (prin which " " (mangle-strict name))
    (if defname (prin " " (mangle-strict defname)))
    (break))
  (assert (even? (length args)) (string/format "expected even number of arguments, got %j" args))
  (prin which " ")
  (if name (prin (mangle-strict name) " "))
  (emit-block-start)
  (each [field ftype] (partition 2 args)
    (emit-indent)
    (emit-type ftype field)
    (print ";"))
  (emit-block-end)
  (if defname (prin " " (mangle-strict defname))))

(defn- emit-struct-def
  [name args defname]
  (emit-struct-union-def "struct" name args defname))

(defn- emit-union-def
  [name args defname]
  (emit-struct-union-def "union" name args defname))

(defn- emit-enum-def
  [name args defname]
  (prin "enum ")
  (if name (prin (mangle-strict name) " "))
  (emit-block-start)
  (each x args
    (emit-indent)
    (if (tuple? x)
      (do
        (prin (x 0) " = ")
        (emit-expression (x 1))
        (print ","))
      (print x ",")))
  (emit-block-end)
  (if defname (prin " " (mangle-strict defname))))

(defn- emit-fn-pointer-type
  [ret-type args defname]
  (prin "(")
  (emit-type ret-type)
  (prin ")(*" (mangle defname) ")(")
  (var is-first true)
  (each x args
    (unless is-first (prin ", "))
    (set is-first false)
    (if (tuple? x)
      (emit-type (x 1) (x 0))
      (emit-type x)))
  (prin ")"))

(defn- emit-ptr-type
  [x alias]
  (emit-type x)
  (prin " *")
  (if alias (prin (mangle alias))))

(defn- emit-ptr-ptr-type
  [x alias]
  (emit-type x)
  (prin " **")
  (if alias (prin (mangle alias))))

(defn- emit-const-type
  [x alias]
  (prin "const ")
  (emit-type x)
  (if alias (prin " " (mangle alias))))

(defn- emit-array-type
  [x n alias]
  (if-not alias (prin "("))
  (emit-type x)
  (if alias (prin " " (mangle alias)))
  (prin "[")
  (when n
    (emit-expression n true))
  (prin "]")
  (if-not alias (prin ")")))

(varfn emit-type
  [definition &opt alias]
  (match definition
    (d (bytes? d)) (do (prin (mangle-strict d)) (if alias (prin " " (mangle alias))))
    (t (tuple? t))
    (match t
      ['struct & body] (emit-struct-def nil body alias)
      ['named-struct n & body] (emit-struct-def n body alias)
      ['enum & body] (emit-enum-def nil body alias)
      ['named-enum n & body] (emit-enum-def n body alias)
      ['union & body] (emit-union-def nil body alias)
      ['named-union n & body] (emit-union-def n body alias)
      ['fn n & body] (emit-fn-pointer-type n body alias)
      ['ptr val] (emit-ptr-type val alias)
      ['* val] (emit-ptr-type val alias)
      ['quote val] (emit-ptr-type val alias) # shorthand
      ['** val] (emit-ptr-ptr-type (definition 1) alias)
      ['const t] (emit-const-type t alias)
      ['array t] (emit-array-type t (get definition 2) alias)
      (errorf "unexpected type form %j" definition))
    (errorf "unexpected type form %j" definition)))

(defn- emit-typedef-impl
  [alias definition]
  (prin "typedef ")
  (emit-type definition alias)
  (print ";"))

# Expressions

(defn- emit-funcall
  [items]
  (def f (get items 0))
  (emit-expression f (symbol? f))
  (prin "(")
  (for i 1 (length items)
    (if (not= i 1) (prin ", "))
    (emit-expression (in items i) true))
  (prin ")"))

(defn- emit-binop
  [op & xs]
  (var is-first true)
  (each x xs
    (if-not is-first (prin " " op " "))
    (set is-first false)
    (emit-expression x)))

(defn- emit-indexer
  [op ds field]
  (emit-expression ds)
  (prin op field))

(defn- emit-unop
  [op x]
  (prin op)
  (emit-expression x))

(defn- emit-ternary
  [c t f]
  (emit-expression c)
  (prin " ? ")
  (emit-expression t)
  (prin " : ")
  (emit-expression f))

(defn- emit-aindex
  [a index]
  (emit-expression a)
  (prin "[")
  (emit-expression index true)
  (prin "]"))

(defn- emit-set
  [lvalue rvalue]
  (emit-expression lvalue true)
  (prin " = ")
  (emit-expression rvalue true))

(defn- emit-deref
  [ptr]
  (prin "*")
  (emit-expression ptr))

(defn- emit-address
  [expr]
  (prin "&")
  (emit-expression expr))

(defn- emit-cast
  [ctype expr]
  (prin "(")
  (emit-type ctype)
  (prin ")")
  (emit-expression expr))

(defn- emit-struct-ctor
  [args]
  (assert (even? (length args)) "expected an even number of arguments for a struct literal")
  (emit-block-start)
  (each [k v] (partition 2 args)
    (emit-indent)
    (prin "." k " = ")
    (emit-expression v true)
    (print ","))
  (emit-block-end))

(defn- emit-array-ctor
  [args]
  (var is-first true)
  (emit-block-start)
  (each x args
    (if-not is-first (print ", "))
    (set is-first false)
    (emit-indent)
    (emit-expression x true))
  (print)
  (emit-block-end))

(varfn emit-expression
  [form &opt noparen]
  #(tracev form)
  (def form (expand-macro :cjanet-expression-macro form))
  #(tracev form)
  (match form
    (f (or (symbol? f) (keyword? f))) (prin (mangle f))
    (n (number? n)) (prinf "%.17g" n)
    (s (string? s)) (prinf "%v" s) # todo - better match escape codes
    (a (array? a)) (do
                     (unless noparen (prin "("))
                     (emit-array-ctor a)
                     (unless noparen (prin ")")))
    (d (dictionary? d))
    (do
      (unless noparen (prin "("))
      (emit-struct-ctor (mapcat identity (sort (pairs d))))
      (unless noparen (print ")")))
    (t (tuple? t))
    (do
      (unless noparen (prin "("))
      (match t
        [(bs (bops bs)) arg1 arg2 & rest] (emit-binop (bops bs) arg1 arg2 ;rest)
        [(bs (uops bs)) & rest] (emit-unop (uops bs) ;rest)
        ['literal l] (prin (string l))
        ['aref v i & more]
          (do (assert (empty? more) "aref expects two arguments") (emit-aindex v i))
        ['call & args] (emit-funcall args)
        ['set v i] (emit-set v i)
        ['deref v] (emit-deref v)
        ['addr v] (emit-address v)
        ['& v] (emit-address v)
        ['splice v] (emit-address v) # hack
        ['quote q] (emit-deref q) # quote looks a bit like "*"
        ['cast t v] (emit-cast t v)
        ['struct & vals] (emit-struct-ctor vals)
        ['array & vals] (emit-array-ctor vals)
        ['-> v f] (emit-indexer "->" v f)
        ['? c t f] (emit-ternary c t f)
        ['. v f] (emit-indexer "." v f)
        (emit-funcall t))
      (unless noparen (prin ")")))
    (b (boolean? b)) (prinf "%j" form)
    (n (nil? n)) (prin "NULL")
    ie (errorf "invalid expression %v" ie)))

# Statements

(defn- emit-declaration
  [binding &opt value]
  (def [v vtype] (type-split binding))
  (emit-type vtype v)
  (when (not= nil value)
    (prin " = ")
    (emit-expression value true)))

(varfn emit-statement
  [form]
  (def form (expand-macro :cjanet-statement-macro form))
  (match form
    ['def & args] (emit-declaration ;args)
    ['var & args] (emit-declaration ;args)
    nil (prin ";")
    (emit-expression form true)))

# Blocks

(defn emit-blocks
  "Emit a number of statements in a bracketed block"
  [statements &opt no-indent]
  (default statements [])
  (unless no-indent (emit-indent))
  (emit-block-start)
  (each s statements
    (emit-block s true))
  (emit-block-end)
  (print))

(defn- emit-cond
  [args]
  (assert (>= (length args) 2) "expected at least 2 arguments to if")
  (var is-first true)
  (each chunk (partition 2 args)
    (def [condition branch] chunk)
    (if (= 1 (length chunk))
      (do
        (prin " else ")
        (emit-block condition))
      (do
        (if is-first
          (do (emit-indent) (prin "if ("))
          (prin " else if ("))
        (set is-first false)
        (emit-expression condition true)
        (prin ") ")
        (emit-block branch))))
  (print))

(defn- emit-while
  [condition stm body]
  (emit-indent)
  (prin "while (")
  (emit-expression condition true)
  (prin ") ")
  (emit-blocks [stm ;body] true)
  (print))

(defn- case-literal? [x] (or (symbol? x) (and (number? x) (= x (math/floor x)))))

(defn- emit-switch
  [condition cases]
  (emit-indent)
  (prin "switch (")
  (emit-expression condition true)
  (prin ") ")
  (emit-block-start)
  (def case-pairs (partition 2 cases))
  (each case-pair case-pairs
    (emit-indent)
    (def [case-value body] case-pair)
    (if (= 1 (length case-pair))
      (do
        (print "default:")
        (emit-block case-value true)
        (print))
      (do
        (prin "case ")
        (assert (case-literal? case-value) "case label must be integer literal or enum")
        (print case-value ":")
        (emit-block body true)
        (print))))
  (emit-block-end)
  (print))

(defn- emit-for
  [init cond step body]
  (emit-indent)
  (prin "for (")
  (emit-statement init)
  (prin "; ")
  (emit-expression cond true)
  (prin "; ")
  (emit-expression step true)
  (prin ") ")
  (emit-blocks body true))

(defn- emit-return
  [v]
  (emit-indent)
  (prin "return ")
  (emit-expression v true)
  (print ";"))

(varfn emit-block
  [form &opt nobracket noindent]
  (def form (expand-macro :cjanet-block-macro form))
  (unless nobracket
    (emit-block-start))
  (match form
    ['do & body] (emit-blocks body)
    ['while cond stm & body] (emit-while cond stm body)
    ['for [init cond step] & body] (emit-for init cond step body)
    ['if & body] (emit-cond body)
    ['switch cond & body] (emit-switch cond body)
    ['cond & body] (emit-cond body)
    ['return val] (emit-return val)
    ['break] (do (unless noindent (emit-indent)) (print "break;"))
    ['continue] (do (unless noindent (emit-indent)) (print "continue;"))
    ['label lab] (print (mangle-strict lab) ":")
    ['goto lab] (do (unless noindent (emit-indent)) (print "goto " (mangle-strict (form 1))))
    stm (do (unless noindent (emit-indent)) (emit-statement stm) (print ";")))
  (unless nobracket (emit-block-end)))

# Top level forms

(defn- emit-storage-classes
  [classes]
  (each class classes
    (prin class " ")))

(defn- emit-function-impl
  [docstring classes name arglist rtype body]
  (print)
  (emit-comment docstring)
  (emit-storage-classes classes)
  (emit-type rtype)
  (prin " " (mangle name) "(")
  (var is-first true)
  (each arg arglist
    (unless is-first (prin ", "))
    (set is-first false)
    (def [v t] (type-split arg))
    (emit-type t v))
  (prin ")")
  (if (empty? body)
    (print ";")
    (do
      (print)
      (emit-blocks body))))

(defn emit-preprocess
  ```
  Emit a line of source code for the pre-processor.
  For example `(emit-preprocess "include" "<stdio.h>")`.
  ```
  [& args]
  (print "#" (string/join (map string args) " ")))

(defn emit-function
  "Emit a C function definition."
  [name & form]
  (def i (index-of '-> form))
  (assert i "invalid function prototype - expected -> before return type")
  (def ret-type (in form (+ i 1)))
  (def arglist (in form (- i 1)))
  (def classes @[])
  (def docstring @"")
  (each meta (slice form 0 (- i 1))
    (case (type meta)
      :keyword (array/push classes meta)
      :symbol (array/push classes meta)
      :string (buffer/push docstring meta)
      (errorf "cannot handle metadata %v - expected keyword, symbol, or string." meta)))
  (def body (tuple/slice form (+ 2 i)))
  (emit-function-impl docstring classes name arglist ret-type body))

(defn emit-declare
  "Emit a declaration of a variable or constant."
  [binding & form]
  (def storage-classes (slice form 0 (dec (length form))))
  (def v (last form))
  (when (next storage-classes)
    (emit-storage-classes storage-classes))
  (emit-declaration binding v)
  (print ";"))

(defn emit-extern
  "Emit a declaration of a variable or constant."
  [binding]
  (emit-declare binding "extern" nil))

(defn emit-typedef
  "Emit a type declaration (C typedef)."
  [name definition]
  (print)
  (emit-typedef-impl name definition))

(defn emit-include
  [path]
  # Add quoting for you
  (def path1 (if (or (string/has-prefix? "<" path) (string/has-prefix? `"` path))
               path
               (string `"` path `"`)))
  (emit-preprocess :include path1))

###
### Top-Level code emitting macros (wrappers around emit-* functions). The macro
### forms quasiquote arguments to make templating easier.
###

(defn- qq-wrap
  [args]
  (map (fn [x] ['quasiquote x]) args))

(defmacro function [& args] ~(,emit-function ,;(qq-wrap args)))
(defmacro preprocess [& args] ~(,emit-preprocess ,;(qq-wrap args)))
(defmacro @ [& args] ~(,emit-preprocess ,;(qq-wrap args)))
(defmacro declare [& args] ~(,emit-declare ,;(qq-wrap args)))
(defmacro extern [& args] ~(,emit-extern ,;(qq-wrap args)))
(defmacro typedef [& args] ~(,emit-typedef ,;(qq-wrap args)))
(defmacro block [& args] ~(,emit-blocks ,(qq-wrap args)))
(defmacro include [path] ~(,emit-include ,;(qq-wrap [path])))

###
### Janet <-> C glue utilities
###

(defdyn *cfun-list* "Array of C Functions defined in the current scope")
(defdyn *cdef-list* "Array of C Constants defined in the current scope")

(def- bindgen-table
  ```
  Store symbols needed to extract or return types for cfunctions.
  Each entry is [alias ctype wrapper-fn getter-fn opt-fn]
  All of the function columns can be nil if that operation is not
  supported for that type when creating bindings.
  ```
  [[:value 'Janet nil 'aref]
   [:any 'Janet nil 'aref]
   [:bool 'int 'janet-wrap-boolean 'janet-getboolean 'janet-optboolean]
   [:nat 'int 'janet-wrap-number 'janet-getnat 'janet-optnat]
   [:int 'int 'janet-wrap-number 'janet-getinteger 'janet-optinteger]
   [:number 'double 'janet-wrap-number 'janet-getnumber 'janet-optnumber]
   [:double 'double 'janet-wrap-number 'janet-getnumber 'janet-optnumber]
   [:float 'float 'janet-wrap-number 'janet-getnumber 'janet-optnumber]
   [:int32 'int32_t 'janet-wrap-number 'janet-getinteger 'janet-optinteger]
   [:int64 'int64_t 'janet-wrap-s64 'janet-getinteger64 'janet-optinteger64]
   [:uint32 'uint32_t 'janet-wrap-number 'janet-getuinteger 'janet-optuinteger]
   [:uint64 'uint64_t 'janet-wrap-u64 'janet-getuinteger64 'janet-optuinteger64]
   [:size 'size_t 'janet-wrap-u64 'janet-getsize 'janet-optsize]
   [:fiber '(* JanetFiber) 'janet-wrap-fiber 'janet-getfiber 'janet-optfiber]
   [:array '(* JanetArray) 'janet-wrap-array 'janet-getarray 'janet-optarray]
   [:tuple 'JanetTuple 'janet-wrap-tuple 'janet-gettuple 'janet-opttuple]
   [:table '(* JanetTable) 'janet-wrap-table 'janet-gettable 'janet-opttable]
   [:struct 'JanetStruct 'janet-wrap-struct 'janet-getstruct 'janet-optstruct]
   [:string 'JanetString 'janet-wrap-string 'janet-getstring nil]
   [:cstring '(const (* char)) 'janet_cstringv 'janet-getcstring 'janet-optcstring]
   [:symbol 'JanetSymbol 'janet-wrap-symbol 'janet-getsymbol 'janet-optsymbol]
   [:keyword 'JanetKeyword 'janet-wrap-keyword 'janet-getkeyword 'janet-optkeyword]
   [:buffer '(* JanetBuffer) 'janet-wrap-buffer 'janet-getbuffer 'janet-optbuffer]
   [:cfunction 'JanetCFunction 'janet-wrap-cfunction 'janet-getcfunction 'janet-optcfunction]
   [:function '(* JanetFunction) 'janet-wrap-function 'janet-getfunction nil]
   [:abstract '(* void) 'janet-wrap-abstract nil nil]
   [:pointer '(* void) 'janet-wrap-pointer 'janet-getpointer 'janet-optpointer]
   [:bytes 'JanetByteView nil 'janet-getbytes nil]
   [:indexed 'JanetView nil 'janet-getindexed nil]
   [:dictionary 'JanetDictView nil 'janet-getdictionary nil]])

# Create convenient to use tables
(def- alias-to-ctype @{})
(def- alias-or-ctype-to-wrap @{})
(def- alias-or-ctype-to-get @{})
(def- alias-or-ctype-to-opt @{})
(each [alias ctype wrapfn getfn optfn] bindgen-table
  (def alias (symbol alias))
  (put alias-to-ctype alias ctype)
  (put alias-or-ctype-to-wrap ctype wrapfn)
  (put alias-or-ctype-to-wrap alias wrapfn)
  (put alias-or-ctype-to-get ctype getfn)
  (put alias-or-ctype-to-get alias getfn)
  (put alias-or-ctype-to-opt ctype optfn)
  (put alias-or-ctype-to-opt alias optfn))

(defn- wrap-v
  "Generate code to wrap any Janet (constant) literal"
  [x]
  (case (type x)
    :nil '(janet_wrap_nil)
    :boolean (if x '(janet_wrap_true) '(janet_wrap_false))
    :number ~(janet_wrap_number ,x)
    :string ~(janet_cstringv ,x)
    :symbol ~(janet_csymbolv ,x)
    :keyword ~(janet_ckeywordv ,x)
    (errorf "cannot emit literal %v" x)))

(defn- return-wrap
  "Generate code to convert return types to a janet value"
  [T code]
  (def wrapfn (get alias-or-ctype-to-wrap T))
  (if wrapfn
    ~(,wrapfn ,code)
    (errorf "cannot convert type %v to a Janet return value" T)))

(defn- janet-get*
  "Get cjanet fragment to extract a given type T into an argument v. The
  parameter is expcted to be in the Janet * argv at index n, no bounds checking needed."
  [binding argv n param-names cparams]
  (def [v T] (type-split binding))
  (def ctype (get alias-to-ctype T T))
  (def getfn (get alias-or-ctype-to-get T))
  (array/push param-names v)
  (array/push cparams [v ctype])
  (if getfn
    ~(def (,v ,ctype) (,getfn ,argv ,n))
    ~(def (,v (* void)) (janet_getabstract ,argv ,n ,T))))

(defn- janet-opt*
  "Get cjanet fragment to extract optional parameters. Similar to non-optional parameters
  but with some differences - for example, container types must provide a default size. Not
  all psuedo-types are supported as optional."
  [binding argv argc n param-names cparams]
  (def [v T dflt] (type-split-dflt binding))
  (def ctype (get alias-to-ctype T T))
  (def optfn (get alias-or-ctype-to-opt T))
  (array/push param-names v)
  (array/push cparams [v ctype])
  (if (in '{value 1 any 1 Janet 1} T)
    ~(def (,v Janet) (? (> argc ,n) (aref ,argv ,n) ,(wrap-v dflt)))
    (if optfn
      ~(def (,v ,ctype) (,optfn ,argv ,argc ,n ,dflt))
      ~(def (,v (* void)) (janet_optabstract ,argv ,argc ,n ,T ,dflt)))))

(defn emit-cfunction
  ```
  Functional form of `cfunction` - takes the same arguments, but parameters must be manually quoted.
  ```
  [name & more]
  (def mangledname (symbol (mangle name)))
  (def docstring @"")
  (def classes @[])
  (def signature (buffer "(" name))
  (def cparams @[])
  (def param-names @[])
  (def cfun-list (if-let [x (dyn *cfun-list*)] x (setdyn *cfun-list* @[])))

  # parse more
  (def i (index-of '-> more))
  (assert i "invalid function prototype - expected -> before return type")
  (def ret-type (in more (+ i 1)))
  (def params (in more (- i 1)))
  (each meta (slice more 0 (- i 1))
    (case (type meta)
      :keyword (array/push classes meta)
      :symbol (array/push classes meta)
      :string (buffer/push docstring meta)
      (errorf "cannot handle metadata %v - expected keyword, symbol, or string." meta)))
  (def body (tuple/slice more (+ 2 i)))

  # Parse params
  (var pcount 0)
  (def argument-parsing @[])
  (var found-optional false)
  (eachp [i p] params
    (if (in '{& true &opt true &named true &keys true} p)
      (set found-optional true)
      (do
        (++ pcount)
        (array/push
          argument-parsing
          (if found-optional
            (janet-opt* p 'argv 'argc (length argument-parsing) param-names cparams)
            (janet-get* p 'argv (length argument-parsing) param-names cparams)))))
    (buffer/format signature " %j" p))
  (def opt-index (index-of '&opt params))
  (def amp-index (index-of '& params))
  (def named-index (index-of '&named params))
  (def keys-index (index-of '&keys params))
  # will ignore the nils since nil is "greater" than all numbers by Janet's total ordering over values
  (def min-arity (or (min opt-index amp-index named-index keys-index) pcount))
  (def max-arity (if (or amp-index named-index keys-index) -1 pcount))
  (buffer/push signature ")")
  # Generate function for use in C
  (emit-function-impl docstring classes mangledname cparams (get alias-to-ctype ret-type ret-type)
                      body)
                      # (eval (qq-wrap body)))
  # Generate wrapper for use in Janet
  (def cfun_name (mangle (string "_generated_cfunction_" mangledname)))
  (print "\nJANET_FN(" cfun_name ",")
  (print "         " (string/format "%j" (string signature)) ", ")
  (print "         " (string/format "%j" (string docstring)) ")")
  (block
    ,(if (= min-arity max-arity)
       ~(janet_fixarity argc ,min-arity)
       ~(janet_arity argc ,min-arity ,max-arity))
    ,;argument-parsing
    (return ,(return-wrap ret-type [mangledname ;param-names])))
  (array/push cfun-list ~(JANET_REG ,(string name) ,(symbol cfun_name)))
  cfun_name)

(defmacro cfunction
  ```
  Define a C Function in cjanet. This also takes care
  of recording docstrings and such. Arity checking will be
  generated for you (by insertion of a call to janet_arity
  or janet_fixarity).
  ```
  [name & more]
  ~(,emit-cfunction ,;(qq-wrap [name ;more])))
  #(emit-cfunction name ;more))

(defn emit-cdef
  ```
  Define constant which will be registered in the module.
  It takes care of the docstring.
  ```
  [name & more]
  (def [docstr body]
    (if-let [ds (and (string? (more 0)) (more 0))
             bo (more 1)]
      [ds bo] [name (more 1)]))
  (def cdef-list (if-let [x (dyn *cdef-list*)] x (setdyn *cdef-list* @[])))
  (array/push cdef-list ~(janet_def env ,(string name) ,body ,docstr))
  nil)

(defmacro cdef
  ```
  Define constant which will be registered in the module.
  It takes care of the docstring.
  ```
  [name & more]
  ~(,emit-cdef ,;(qq-wrap [name ;more])))

(defn emit-module-entry
  "Call this at the end of a cjanet module to add a module entry function."
  [name]
  (def all-cfuns (dyn *cfun-list* @[]))
  (def all-cdefs (dyn *cdef-list* @[]))
  (prin "\nJANET_MODULE_ENTRY(JanetTable *env) ")
  (block
    ,;all-cdefs
    (def (cfuns (array JanetRegExt)) (array ,;all-cfuns JANET_REG_END))
    (janet_cfuns_ext env ,name cfuns)))

(defmacro module-entry
  "Call this at the end of a cjanet module to add a module entry function."
  [name]
  (emit-module-entry name))

###
### "JIT" functionality for use in repl and to reduce boilerplate
###

(defdyn *jit-context* "A context value for storing the current state of compilation, including buffers, flags, and tool paths.")

(defn begin-jit
  ```
  Begin C Janet JIT context. Optionally pass in options to configure compilation. The `options` argument
  will be passed to the `spork/cc` module to compile generated C code. Generated intermediates will be created
  in the _cjanet/ directory.
  ```
  [&keys options]
  (def compilation-unit @"#include <janet.h>\n")
  (def prevout (dyn *out*))
  (def cont
    {:buffer compilation-unit
     :build-dir "_cjanet"
     :opts options
     :old-out prevout
     :module-name (get options :module-name (string "cjanet_" (os/getpid)))})
  (setdyn *jit-context* cont)
  (os/mkdir "_cjanet")
  (setdyn *out* compilation-unit)
  cont)

(defn end-jit
  ```
  End current compilation context, compile all buffered code, and then by default load it into the current process.
  The `no-load` argument controls whether or not the compiled code is loaded. If `no-load` is truthy, then
  this function will return the path to the compiled shared object and skip loading.
  If `cache` is truthy, this function will use a previously compiled shared object or DLL if it exists and the source code matches.
  ```
  [&named no-load cache]

  # 0. Unpack context
  (def ccontext (assert (dyn *jit-context*)))
  (def module-name (assert (get ccontext :module-name)))
  (def builddir (assert (get ccontext :build-dir)))
  (def opts (get ccontext :opts {}))
  (def buf (assert (get ccontext :buffer)))
  (def prevout (get ccontext :old-out))
  (def toolchain (pm-config/detect-toolchain (curenv)))

  # 1. Create module entry
  (emit-module-entry module-name)

  # 2. Reset old context
  (setdyn *jit-context* nil)
  (setdyn *out* prevout)

  # 3. Emit C source code
  (os/mkdir builddir)
  (def name (string builddir "/" module-name))
  (def c-source (string name ".c"))
  (if cache
    (do
      (def [has-old old-source] (protect (slurp c-source)))
      (unless (deep= old-source buf)
        (spit c-source buf)))
    (spit c-source buf))
  (when (get opts :eprint-source) (eprint buf)) # debug
  (buffer/clear buf)
  (buffer/trim buf) # save mem

  # 4. Compile to shared object
  (var so (string name ".so"))
  (with-dyns []
    (def env (curenv))
    (eachp [k v] opts (setdyn k v))
    # These cannot be overriden
    (setdyn cc/*visit* cc/visit-execute-if-stale)
    (setdyn cc/*build-dir* builddir)
    (when-let [pc (get opts :pkg-config)]
      (cc/pkg-config ;pc))
    (if (= :msvc toolchain)
      (do
        (set so (string name ".dll"))
        (put env cc/*lflags* @[;(get env cc/*lflags* @[]) "/NOIMPLIB" (cc/msvc-janet-import-lib)])
        (cc/msvc-compile-and-link-shared so c-source))
      (do
        (cc/compile-and-link-shared so c-source))))

  # 5. Import shared object
  (if no-load
    so
    (native so (curenv))))
