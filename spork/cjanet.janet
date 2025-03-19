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

(def- mangle-peg
  (peg/compile
    ~{:valid (range "az" "AZ" "__")
      :one (+ (/ "-" "_") ':valid (/ '1 ,|(string "_X" ($ 0))))
      :main (% (* :one (any (+ ':d :one))))}))

(def- bops
  {'+ '+ '- '- '* '* '/ '/ '% '% '< '<
   '> '> '<= '<= '>= '>= '== '== '!= '!=
   'not= "!="
   '>> ">>" '<< "<<" '&& "&&" '^ "^"
   'and "&&" 'or "||" 'band "&" 'bor "|" 'bxor "^" 'set "="
   'blshift "<<" 'brshift ">>"})

(def- uops {'bnot "~" 'not "!" 'neg "-" '! "!" '++ "++" '-- "--"})

(defn mangle
  "Convert any sequence of bytes to a valid C identifier in a way that is unlikely to collide.
  `print-ir` will not mangle symbols for you."
  [token]
  (first (peg/match mangle-peg token)))

(def- type-split-peg
  (peg/compile '(* (? (* '(to ":") ":")) '(any 1))))

(defn type-split
  "Extract name and type from a variable. Allow typing variables as both
  (name type) or name:type as a shorthand. If no type is found, default to dflt-type. dflt-type
  itself defaults to 'auto"
  [x &opt dflt-type]
  (default dflt-type 'auto)
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
    (prin which " " name)
    (if defname (prin " " defname))
    (break))
  (assert (even? (length args)) (string/format "expected even number of arguments, got %j" args))
  (prin which " ")
  (if name (prin name " "))
  (emit-block-start)
  (each [field ftype] (partition 2 args)
    (emit-indent)
    (emit-type ftype field)
    (print ";"))
  (emit-block-end)
  (if defname (prin " " defname)))

(defn- emit-struct-def
  [name args defname]
  (emit-struct-union-def "struct" name args defname))

(defn- emit-union-def
  [name args defname]
  (emit-struct-union-def "union" name args defname))

(defn- emit-enum-def
  [name args defname]
  (prin "enum ")
  (if name (prin name " "))
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
  (if defname (prin " " defname)))

(defn- emit-fn-pointer-type
  [ret-type args defname]
  (prin "(")
  (emit-type ret-type)
  (prin ")(*" defname ")(")
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
  (if alias (prin alias)))

(defn- emit-ptr-ptr-type
  [x alias]
  (emit-type x)
  (prin " **")
  (if alias (prin alias)))

(defn- emit-const-type
  [x alias]
  (prin "const ")
  (emit-type x)
  (if alias (prin " " alias)))

(defn- emit-array-type
  [x n alias]
  (if-not alias (prin "("))
  (emit-type x)
  (if alias (prin " " alias))
  (prin "[")
  (when n
    (emit-expression n true))
  (prin "]")
  (if-not alias (prin ")")))

(varfn emit-type
  [definition &opt alias]
  (match definition
    (d (bytes? d)) (do (prin d) (if alias (prin " " alias)))
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
      ['ptrptr val] (emit-ptr-ptr-type val alias)
      ['** val] (emit-ptr-ptr-type (definition 1) alias)
      ['const t] (emit-const-type t alias)
      ['array t] (emit-array-type t (get definition 2) alias)
      (errorf "unexpected type form %v" definition))
    (errorf "unexpected type form %v" definition)))

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
  (prin "(" ctype ")")
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
  (match form
    (f (or (symbol? f) (keyword? f))) (prin f)
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
        [(bs (bops bs)) & rest] (emit-binop (bops bs) ;rest)
        [(bs (uops bs)) & rest] (emit-unop (uops bs) ;rest)
        ['literal l] (prin (string l))
        ['quote q] (prin (string q))
        ['aref v i] (emit-aindex v i)
        ['call & args] (emit-funcall args)
        ['set v i] (emit-set v i)
        ['deref v] (emit-deref v)
        ['addr v] (emit-address v)
        ['cast t v] (emit-cast t v)
        ['struct & vals] (emit-struct-ctor vals)
        ['array & vals] (emit-array-ctor vals)
        ['-> v f] (emit-indexer "->" v f)
        ['? c t f] (emit-ternary c t f)
        ['. v f] (emit-indexer "." v f)
        (emit-funcall t))
      (unless noparen (prin ")")))
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
  (match form
    ['def & args] (emit-declaration ;args)
    (emit-expression form true)))

# Blocks

(defn emit-blocks
  "Emit a number of statements in a bracketed block"
  [statements]
  (when (one? (length statements))
    (emit-block (get statements 0))
    (break))
  (emit-indent)
  (emit-block-start)
  (each s statements
    (emit-block s true))
  (emit-block-end)
  (print))

(defn- emit-cond
  [args]
  (assert (>= (length args) 2) "expected at least 2 arguments to if")
  (var is-first true)
  (each [condition branch] (partition 2 args)
    (if (= nil branch)
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
  (emit-blocks [stm ;body])
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
  (emit-expression init true)
  (prin "; ")
  (emit-expression cond true)
  (prin "; ")
  (emit-expression step true)
  (prin ") ")
  (emit-blocks body)
  (print))

(defn- emit-return
  [v]
  (emit-indent)
  (prin "return ")
  (emit-expression v true)
  (print ";"))

(varfn emit-block
  [form &opt nobracket]
  (unless nobracket
    (emit-block-start))
  (match form
    ['do & body] (emit-blocks body)
    ['while cond stm & body] (emit-while cond stm body)
    ['for [init cond step] & body] (emit-for init cond step body)
    ['switch cond & body] (emit-switch cond body)
    ['if & body] (emit-cond body)
    ['cond & body] (emit-cond body)
    ['return val] (emit-return val)
    ['break] (do (emit-indent) (print "break;"))
    ['continue] (do (emit-indent) (print "continue;"))
    ['label lab] (print "label " lab ":")
    ['goto lab] (do (emit-indent) (print "goto " (form 1)))
    stm (do (emit-indent) (emit-statement stm) (print ";")))
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
  (prin " " name "(")
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

(defn emit-typedef
  "Emit a type declaration (C typedef)."
  [name definition]
  (print)
  (emit-typedef-impl name definition))

(defn emit-include
  [path]
  (emit-preprocess :include path))

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
(defmacro typedef [& args] ~(,emit-typedef ,;(qq-wrap args)))
(defmacro block [& args] ~(,emit-blocks ,(qq-wrap args)))
(defmacro include [path] ~(,emit-include ,;(qq-wrap [path])))

###
### Janet <-> C glue utilities
###

(defdyn *cfun-list* "Array of C Functions defined in the current scope")
(defdyn *cdef-list* "Array of C Constants defined in the current scope")

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
  (case (keyword T)
    :value code
    :any code
    :Janet code
    :number ~(janet_wrap_number ,code)
    :double ~(janet_wrap_number ,code)
    :float ~(janet_wrap_number ,code)
    :int ~(janet_wrap_number ,code)
    :nat ~(janet_wrap_number ,code)
    :int32 ~(janet_wrap_number ,code)
    :int64 ~(janet_wrap_s64 ,code)
    :uint64 ~(janet_wrap_u64 ,code)
    :size ~(janet_wrap_u64 ,code)
    :fiber ~(janet_wrap_fiber ,code)
    :array ~(janet_wrap_array ,code)
    :tuple ~(janet_wrap_tuple ,code)
    :table ~(janet_wrap_table ,code)
    :struct ~(janet_wrap_struct ,code)
    :string ~(janet_wrap_string ,code)
    :cstring ~(janet_cstringv ,code)
    :symbol ~(janet_wrap_symbol ,code)
    :keyword ~(janet_wrap_keyword ,code)
    :buffer ~(janet_wrap_buffer ,code)
    :cfunction ~(janet_wrap_cfunction ,code)
    :function ~(janet_wrap_function ,code)
    :bool ~(janet_wrap_boolean ,code)
    :pointer ~(janet_wrap_pointer ,code)
    :asbtract ~(janet_wrap_abstract ,code)
    (errorf "cannot convert type %v to a Janet return value" T)))


(def- type-alias-to-ctype
  {:value 'Janet
   :any 'Janet
   :Janet 'Janet
   :number 'double
   :double 'double
   :float 'float
   :int 'int
   :nat 'int32_t
   :int32 'int32_t
   :int64 'int64_t
   :uint64 'uint64_t
   :size 'size_t
   :fiber '(* JanetFiber)
   :array '(* JanetArray)
   :tuple 'JanetTuple
   :table '(* JanetTable)
   :struct 'JanetStruct
   :string 'JanetString
   :cstring '(const (* char))
   :symbol 'JanetSymbol
   :keyword 'JanetKeyword
   :buffer '(* JanetBuffer)
   :cfunction 'JanetCFunction
   :function '(* JanetFunction)
   :bool 'int
   :pointer '(* void)
   :bytes 'JanetByteView
   :indexed 'JanetView
   :dictionary 'JanetDictView})

(defn- janet-get*
  "Get cjanet fragment to extract a given type T into an argument v. The
  parameter is expcted to be in the Janet * argv at index n, no bounds checking needed."
  [binding argv n param-names cparams]
  (def [v T] (type-split binding))
  (array/push param-names v)
  (array/push cparams [v (get type-alias-to-ctype (keyword T) '(* void))])
  (case (keyword T)
    :value ~(def (,v Janet) (aref ,argv ,n))
    :any ~(def (,v Janet) (aref ,argv ,n))
    :Janet ~(def (,v Janet) (aref ,argv ,n))
    :number ~(def (,v double) (janet_getnumber ,argv ,n))
    :double ~(def (,v double) (janet_getnumber ,argv ,n))
    :float ~(def (,v float) (janet_getnumber ,argv ,n))
    :int ~(def (,v int) (janet_getinteger ,argv ,n))
    :nat ~(def (,v int32_t) (janet_getnat ,argv ,n))
    :int32 ~(def (,v int32_t) (janet_getinteger ,argv ,n))
    :int64 ~(def (,v int64_t) (janet_getinteger64 ,argv ,n))
    :uint64 ~(def (,v uint64_t) (janet_getuinteger64 ,argv ,n))
    :size ~(def (,v size_t) (janet_getsize ,argv ,n))
    :fiber ~(def (,v (* JanetFiber)) (janet_getfiber ,argv ,n))
    :array ~(def (,v (* JanetArray)) (janet_getarray ,argv ,n))
    :tuple ~(def (,v JanetTuple) (janet_gettuple ,argv ,n))
    :table ~(def (,v (* JanetTable)) (janet_gettable ,argv ,n))
    :struct ~(def (,v JanetStruct) (janet_getstruct ,argv ,n))
    :string ~(def (,v JanetString) (janet_getstring ,argv ,n))
    :cstring ~(def (,v (const (* char))) (janet_getcstring ,argv ,n))
    :symbol ~(def (,v JanetSymbol) (janet_getsymbol ,argv ,n))
    :keyword ~(def (,v JanetKeyword) (janet_getkeyword ,argv ,n))
    :buffer ~(def (,v (* JanetBuffer)) (janet_getbuffer ,argv ,n))
    :cfunction ~(def (,v JanetCFunction) (janet_getcfunction ,argv ,n))
    :function ~(def (,v (* JanetFunction)) (janet_getfunction ,argv ,n))
    :bool ~(def (,v int) (janet_getboolean ,argv ,n))
    :pointer ~(def (,v (* void)) (janet_getpointer ,argv ,n))
    :bytes ~(def (,v JanetByteView) (janet_getbytes ,argv ,n))
    :indexed ~(def (,v JanetView) (janet_getindexed ,argv ,n))
    :dictionary ~(def (,v JanetDictView) (janet_getdictionary ,argv ,n))
    # default - must be abstract
    (do
      ~(def (,v (* void)) (janet_getabstract ,argv ,n ,T)))))

(defn- janet-opt*
  "Get cjanet fragment to extract optional parameters. Similar to non-optional parameters
  but with some differences - for example, container types must provide a default size. Not
  all psuedo-types are supported as optional."
  [binding argv argc n param-names cparams]
  (def [v T dflt] (type-split-dflt binding))
  (array/push param-names v)
  (array/push cparams [v (get type-alias-to-ctype (keyword T) '(* void))])
  (case (keyword T)
    :value ~(def (,v Janet) (? (> argc ,n) (aref ,argv ,n) ,(wrap-v dflt)))
    :any ~(def (,v Janet) (? (> argc ,n) (aref ,argv ,n) ,(wrap-v dflt)))
    :Janet ~(def (,v Janet) (? (> argc ,n) (aref ,argv ,n) ,(wrap-v dflt)))
    :number ~(def (,v double) (janet_optnumber ,argv ,argc ,n ,dflt))
    :double ~(def (,v double) (janet_optnumber ,argv ,argc ,n ,dflt))
    :float ~(def (,v float) (janet_optnumber ,argv ,argc ,n ,dflt))
    :int ~(def (,v int) (janet_optinteger ,argv ,argc ,n ,dflt))
    :nat ~(def (,v int32_t) (janet_optnat ,argv ,argc ,n ,dflt))
    :int32 ~(def (,v int32_t) (janet_optinteger ,argv ,argc ,n ,dflt))
    :int64 ~(def (,v int64_t) (janet_optinteger64 ,argv ,n))
    :uint64 ~(def (,v uint64_t) (janet_getuinteger64 ,argv ,argc ,n ,dflt))
    :size ~(def (,v size_t) (janet_optsize ,argv ,argc ,n ,dflt))
    :array ~(def (,v (* JanetArray)) (janet_optarray ,argv ,argc ,n ,dflt))
    :table ~(def (,v (* JanetTable)) (janet_opttable ,argv ,argc ,n ,dflt))
    :cstring ~(def (,v (const (* char))) (janet_optcstring ,argv ,argc ,n ,dflt))
    :buffer ~(def (,v (* JanetBuffer)) (janet_optbuffer ,argv ,argc ,n ,dflt))
    :cfunction ~(def (,v JanetCFunction) (janet_optcfunction ,argv ,argc ,n ,dflt))
    :bool ~(def (,v int) (janet_optboolean ,argv ,argc ,n ,dflt))
    :pointer ~(def (,v (* void)) (janet_optpointer ,argv ,argc ,n ,dflt))
    (do
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
  (emit-function-impl docstring classes mangledname cparams (get type-alias-to-ctype (keyword ret-type))
                      (eval (qq-wrap body)))
  # Generate wrapper for use in Janet
  (def cfun_name (mangle (string "_generated_cfunction_" mangledname)))
  (print "\nJANET_FN(" cfun_name ",")
  (print "        " (string/format "%j" (string signature)) ", ")
  (print "        " (string/format "%j" (string docstring)) ")")
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
  (emit-cfunction name ;more))

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
  (emit-cdef name ;more))

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
