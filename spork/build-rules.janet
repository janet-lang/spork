###
### spork/build-rules.janet
###
### Run commands that produce files in an incremental manner.
### Use to implement a build system.
###

(defn- cancel-all [fibers reason] (each f fibers (ev/cancel f reason) (put fibers f nil)))

(defn- wait-for-fibers
  [chan fibers]
  (defer (cancel-all fibers "parent canceled")
    (repeat (length fibers)
      (def [sig fiber] (ev/take chan))
      (if (= sig :ok)
        (put fibers fiber nil)
        (do
          (cancel-all fibers "sibling canceled")
          (propagate (fiber/last-value fiber) fiber))))))

(defn- target-not-found
  "Creates an error message."
  [target]
  (errorf "target %v does not exist and no rule exists to build it" target))

(defn- target-already-defined
  "Error when an output already has a rule defined to create it."
  [target]
  (errorf "target %v has multiple rules" target))

(defn- stat-mtime
  "Cache modified times to make as few calls to os/stat as possible"
  [path mtime-cache]
  (if-let [check (get mtime-cache path)]
    check
    (set (mtime-cache path)
         (let [m (os/stat path :modified)]
           (if (nil? m) false m)))))

(defn- utd
  "Check if a target is up to date."
  [target all-targets utd-cache mtime-cache]
  (def u (get utd-cache target))
  (if (not= nil u) (break u))
  (def rule (get all-targets target))
  (if (= target (get rule :task)) (break (set (utd-cache target) false)))
  (def mtime (stat-mtime target mtime-cache))
  (if-not mtime
    (if rule
      (break (set (utd-cache target) false))
      (target-not-found target)))
  (var ret true)
  (each i (get rule :inputs [])
    (if-not (utd i all-targets utd-cache mtime-cache) (break (set ret false)))
    (def s (stat-mtime i mtime-cache))
    (when (or (not s) (< mtime s))
      (set ret false)
      (break)))
  (set (utd-cache target) ret))

(defn- run-rules
  "Execute the minimal set of rules needed to build all targets in `targets`."
  [rules targets &opt n-workers]
  (def utd-cache @{})
  (def all-targets @{})
  (def dirty-targets @{})
  (def dependents @{})
  (def dep-counts @{})
  (def mtime-cache @{})
  (var work-count 0)
  (def targets-built @[])
  (def q (ev/chan math/int32-max))

  # Check rules for duplicates
  (each rule (distinct rules)
    (when-let [p (get rule :task)]
      (when (get all-targets p) (target-already-defined p))
      (put all-targets p rule))
    (each o (get rule :outputs [])
      (when (get all-targets o) (target-already-defined o))
      (put all-targets o rule)))

  # Check for rules that need running
  (defn needs-build? [target]
    (def check (get dirty-targets target))
    (if (not= nil check) (break check))
    (def rule (get all-targets target))
    (def inputs (get rule :inputs []))
    (var needs-build (not (utd target all-targets utd-cache mtime-cache)))
    (var dep-count 0)
    (each i inputs
      (put dependents i (put (get dependents i @{}) target true))
      (when (needs-build? i) (++ dep-count) (set needs-build true)))
    (put dep-counts target dep-count)
    (put dirty-targets target needs-build)
    (when needs-build
      (if (= 0 dep-count) (ev/give q target))
      (++ work-count))
    needs-build)
  (each target targets (needs-build? target))
  (default n-workers 1)

  (defn worker
    [n]
    (while (pos? work-count)
      (def target (ev/take q))
      (if-not target (break))
      (-- work-count)
      (def rule (get all-targets target))
      (def dependent-set (get dependents target ()))
      (def r (assert (get rule :recipe)))
      (edefer
        (do
          (each o (get rule :outputs [])
            (protect (os/rm o)))
          (repeat n-workers (ev/give q nil)))
        (if (indexed? r)
          (each rr r (rr))
          (r)))
      (array/push targets-built target)
      (eachk next-target dependent-set
        (-- (dep-counts next-target))
        (if (= 0 (get dep-counts next-target))
          (ev/give q next-target))))
    (ev/give q nil))

  (def fibers @{})
  (def super (ev/chan))
  (forv i 0 n-workers
    (def fib (ev/go worker i super))
    (put fibers fib fib))
  (wait-for-fibers super fibers)

  targets-built)

(defn- gettarget [rules target]
  (def item (get rules target))
  (unless item (error (string "no rule for target '" target "'")))
  item)

(defn- target-append
  [rules target key v]
  (def item (gettarget rules target))
  (def vals (get item key))
  (unless (find |(= v $) vals)
    (array/push vals v))
  item)

(defn- rule-impl
  [rules target deps thunk]
  (def phony (keyword? target))
  (assert (table? rules) "rules must be a table")
  (def all-targets
    (cond
      (keyword? target) [(string target)]
      (string? target) [target]
      (indexed? target) target
      (errorf "bad target %v" target)))
  (def target (first all-targets))
  (each d [;deps ;all-targets]
    (assert (string? d) "inputs and outputs must be strings"))
  (unless (get rules target)
    (def new-rule
      @{:inputs @[]
        :outputs @[]
        :recipe @[]})
    (put rules target new-rule))
  (each d deps (target-append rules target :inputs d))
  (if phony
    (put (gettarget rules target) :task target)
    (each t all-targets (target-append rules target :outputs t)))
  (target-append rules target :recipe thunk))

(defmacro build-rule
  ```
  Add a rule to the rule graph. `rules` should be a table, `target`
  a string or tuple of strings, and `deps` a tuple of strings. `body`
  is code that will be executed to create all of the targets by the rules.
  If target is a keyword, the rule will always be considered out of date.
  ```
  [rules target deps & body]
  ~(,rule-impl ,rules ,target ,deps (fn :build-rule [] nil ,;body)))

(defn build-thunk
  ```
  Add a rule to the rule graph. `rules` should be a table, `target`
  a string or tuple of strings, and `deps` a tuple of strings. `body`
  is code that will be executed to create all of the targets by the rules.
  If target is a keyword, the rule will always be considered out of date.
  ```
  [rules target deps thunk]
  (rule-impl rules target deps thunk))

(defn build-run
  "Build a list of targets, as specified by rules. Return an array of all recursively updated targets."
  [rules targets &opt n-workers]
  (assert (table? rules) "rules must be a table")
  (default n-workers (os/cpu-count))
  (def all-targets (if (indexed? targets) targets [targets]))
  (run-rules rules all-targets n-workers))
