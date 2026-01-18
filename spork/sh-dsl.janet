###
### Command Line Shell DSL
### Inexact port of janet-sh
###
### sh-dsl.janet
###

(def- parse-env-set-peg (peg/compile '(* '(to "=") "=" ':S*)))

# Re-implements ev/go-gather

(defn- cancel-all [chan fibers reason]
  (each f fibers (ev/cancel f reason))
  (let [n (length fibers)]
    (table/clear fibers)
    (repeat n (ev/take chan))))

(defn- wait-for-fibers
  [chan fibers]
  (defer (cancel-all chan fibers "parent canceled")
    (repeat (length fibers)
      (def [sig fiber] (ev/take chan))
      (if (= sig :ok)
        (put fibers fiber nil)
        (do
          (cancel-all chan fibers "sibling canceled")
          (propagate (fiber/last-value fiber) fiber))))))

(defn- wait-thunks
  [thunks]
  (def fset @{})
  (def chan (ev/chan))
  (def results @[])
  (each thunk thunks
    (def ftemp (ev/go thunk nil chan))
    (array/push results ftemp)
    (put fset ftemp ftemp))
  (wait-for-fibers chan fset)
  (map fiber/last-value results))

# End ev/go-gather

(defn- string-token [x]
  (if (bytes? x)
    (string x)
    (string/format "%j" x)))

(defn- drop-newline
  [buf]
  (if (= (chr "\n") (last buf)) (buffer/popn buf 1))
  (if (= (chr "\r") (last buf)) (buffer/popn buf 1))
  buf)

(compwhen (= (os/which) :windows)
  # Try the share read (:R) flag on windows
  (defn- get-write-fd [path] (os/open path :wctRIV))
  (defn- get-read-fd [path] (os/open path :rbRIV))
  (defn- get-append-fd [path] (os/open path :caRIV)))

# The os/open verions _should_ work better on windows.
(compwhen (not= (os/which) :windows)
  (defn- get-write-fd [path] (os/open path :wct))
  (defn- get-read-fd [path] (os/open path :r))
  (defn- get-append-fd [path] (os/open path :wca)))

(defn- do-pipeline-impl
  ```
  Build the final pipeline from parsed tokens. Creates pipes
  and opens files, calls os/spawn on each command, and finally waits
  until completion. Also cleans up resources when done.
  ```
  [pipeline &opt capture return-all]
  (def procs @[])
  (def fds @[])
  (var pipein nil)
  (defn getfd [f] (array/push fds f) f)

  (defer
    (each f fds (:close f)) # Don't leak descriptors
    (eachp [i {:cmd cmd :tab t}] pipeline
      (def is-last (= i (dec (length pipeline))))
      (def to-close @[])
      (var pipe-w nil)
      (when pipein
        (put t :in pipein)
        (array/push to-close pipein))

      # Handle pipes
      (unless (and is-last (not capture))
        (def [r w] (os/pipe (if (and is-last capture) :W :WR)))
        (put t :out w)
        (array/push to-close r w)
        (set pipe-w (getfd w))
        (set pipein (getfd r)))

      # Create file descriptors for inputs and outputs.
      (each key [:out :err :in]
        (when (string? (t key))
          (def getter (if (= key :in) get-read-fd get-write-fd))
          (set (t key) (getfd (getter (t key))))
          (array/push to-close (t key))))

      # Append versions
      (each key [:out-append :err-append]
        (when (string? (t key))
          (def tkey (keyword/slice key 0 3))
          (set (t tkey) (getfd (get-append-fd (t key))))
          (array/push to-close (t tkey))))

      # Redirect stderr to stdout - work-around for older janet versions before native support
      (if (= (get t :err) :out) (set (t :err) (get t :out stdout)))

      # Make env table to pass to os/spawn
      (def has-env-var (get t :has-envvar))
      (def finalt
        (if has-env-var
          (merge-into (os/environ) t)
          t))

      # Create process
      (def proc (os/spawn (map string-token cmd) (if has-env-var :pe :p) finalt))
      (getfd proc)

      # Make array of all spawned processes that we can query
      (array/push procs [proc to-close]))

    (def thunks
      (seq [[p to-close] :in procs]
        (fn :pipeline []
          (def res (os/proc-wait p))
          (each x to-close (:close x))
          res)))

    (when capture
      (def final-pipe pipein)
      (array/push
        thunks
        (fn :read-all []
          (def capture-buf @"")
          (ev/read final-pipe :all capture-buf)
          capture-buf)))

    (def out (wait-thunks thunks))
    (if return-all out (last out))))

###
### DSL Parsing
###

(defn- parse-token
  [t]
  (match t
    # Env variable
    (s (and (symbol? s) (= (in s 0) (chr "$"))))
    ~(,os/getenv ,(string/slice t 1))
    # Binding
    ['quote x] t
    ['unquote x] x
    # Tuple shorthand
    (x (and (tuple? x) (= (tuple/type x) :parens)))
    x
    # Normal token
    x
    (string-token x)))

(defn- build-pipeline
  [tokens]
  (def pipeline @[])
  (def cmd-buffer @[])
  (def tab @{})
  (var tabkey nil)

  (defn emit-token [x]
    (array/push cmd-buffer x))

  (defn next-cmd
    []
    (def cmd (array/slice cmd-buffer))
    (def cmd-tab (table/clone tab))
    (array/push pipeline
                {:cmd cmd
                 :tab cmd-tab})
    (array/clear cmd-buffer)
    (table/clear tab))

  # Look at tokens including flow control tokens
  (defn look-token
    [t]
    (match t
      # Input
      '< (set tabkey :in)
      '<in (set tabkey :in)
      # Output and Error pipes
      '> (do (set tabkey :out) (put tab :out-append nil))
      '>out (do (set tabkey :out) (put tab :out-append nil))
      '>> (do (set tabkey :out-append) (put tab :out nil))
      '>>out (do (set tabkey :out-append) (put tab :out nil))
      '>err (do (set tabkey :err) (put tab :err-append nil))
      '>>err (do (set tabkey :err-append) (put tab :err nil))
      # Error-to-Out
      '>err-to-out (do (put tab :err :out) (put tab :err-append nil))
      :err-to-out (do (put tab :err :out) (put tab :err-append nil))
      # Env binding set
      (s (and (empty? cmd-buffer) (bytes? s) (peg/match parse-env-set-peg s)))
      (let [[k v] (peg/match parse-env-set-peg s)]
        (put tab :has-envvar true)
        (put tab k (parse-token (if (symbol? s) (symbol v) v))))
      # Pipe
      ['short-fn x]
      (do (unless (empty? cmd-buffer) (next-cmd)) (look-token x))
      # Default
      x (emit-token (parse-token x))))

  # Parse tokens and build pipeline data
  (each t tokens
    (if tabkey
      (do
        (set (tab tabkey) (parse-token t))
        (set tabkey nil))
      (look-token t)))

  # Handle last cmd
  (unless (empty? cmd-buffer) (next-cmd))

  # Process and check pipeline
  (eachp [i {:tab t}] pipeline
    (def is-first (zero? i))
    (def is-last (= i (dec (length pipeline))))
    (unless is-first
      (assert (not (get t :in)) "command cannot have file input in pipeline stage"))
    (unless is-last
      (assert (not (get t :out)) "command cannot have file output in pipeline stage")))

  pipeline)

(defn- dsl-impl
  [tokens &opt capture all-status]
  # Run pipeline at runtime
  ~(,do-pipeline-impl ,(build-pipeline tokens) ,capture ,all-status))

###
### API
###

(defn run-pipeline
  ```
  Run a quoted pipeline object. Will return the exit code of all commands in the pipeline. If :capture-output is truthy, will also append
  the standard out of the last command in the pipeline.
  ```
  [pipeline &named capture-output]
  (do-pipeline-impl (build-pipeline pipeline) capture-output true))

(defmacro $
  "Run and return exit codes of each command in the pipeline. The last status code is the what is used generally for sucess/failure testing."
  [& cmd]
  (dsl-impl cmd false))

(defmacro $?
  "Run and return true if last command passed, false otherwise"
  [& cmd]
  ~(,zero? ,(dsl-impl cmd false)))

(defmacro $<_
  "Run and returns the standard output of the last command in the output with the last newline stripped."
  [& cmd]
  ~(,drop-newline ,(dsl-impl cmd true)))

(defmacro $<
  "Run and returns the standard output of the last command in the output"
  [& cmd]
  (dsl-impl cmd true))
