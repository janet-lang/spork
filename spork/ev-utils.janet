###
### ev-utils.janet
###
### Module for parallel execution utilities with Janet.
###

(defmacro wait-cancel
  "Wait forever until the current fiber is canceled, and then run some cleanup code."
  [& body]
  ~(as-macro ,defer (do ,;body) (while true (,ev/sleep 1024))))

(defn nursery
  "Group a number of fibers into a single object for structured concurrency"
  []
  @{:supervisor (ev/chan) :fibers @{}})

(defn go-nursery
  "Spawn a fiber into a nursery, similar to ev/go."
  [nurse f &opt value]
  (def super (get nurse :supervisor))
  (def fibs (get nurse :fibers))
  (def fib (ev/go f value super))
  (set (fibs fib) fib))

(defmacro spawn-nursery
  "Similar to ev/spawn but associate spawned fibers with a nursery"
  [nurse & body]
  ~(,go-nursery ,nurse (fn _spawn [&] ,;body)))

(defn- drain-fibers
  "Canceling a group of fibers and wait for them all to complete."
  [super fibers reason]
  (each f fibers (ev/cancel f reason))
  (def n (length fibers))
  (table/clear fibers)
  (repeat n (ev/take super)))

(defn join-nursery
  "Suspend the current fiber until the nursery is emptied."
  [nurse]
  (def fibs (get nurse :fibers))
  (def super (get nurse :supervisor))
  (defer (drain-fibers super fibs "parent canceled")
    (while (next fibs)
      (def [sig fiber] (ev/take super))
      (if (= sig :ok)
        (put fibs fiber nil)
        (do
          (drain-fibers super fibs "sibling canceled")
          (propagate (fiber/last-value fiber) fiber))))))

(defn- join
  "Special case of supervise for implementing some parallel functions."
  [supervisor fibers]
  (var err-fiber nil)
  (defer (drain-fibers supervisor fibers "parent canceled")
    (while (next fibers)
      (def [sig fiber] (ev/take supervisor))
      (if (= sig :ok)
        (put fibers fiber nil)
        (do
          (drain-fibers supervisor fibers "sibling canceled")
          (propagate (fiber/last-value fiber) fiber))))))

(defn pcall
  "Call a function n times (in parallel) for side effects.
  Each function is called with an integer argument indicating a fiber index. Returns nil."
  [f n]
  (assert (> n 0))
  (def chan (ev/chan))
  (def new-f (if (function? f) f (fn [x] (f x))))
  (join chan
        (tabseq [i :range [0 n]
                 :let [fib (ev/go (fiber/new new-f :tp) i chan)]]
                fib fib)))

(defn pmap-full
  "Function form of `ev/gather`. If any of the
  sibling fibers error, all other siblings will be canceled.  Returns the gathered
  results in an array. `data` can be any indexed data structure."
  [f data]
  (def chan (ev/chan))
  (def res (if (dictionary? data) @{} @[]))
  (join chan
        (seq [[i x] :pairs data]
          (ev/go (fiber/new (fn [] (put res i (f x))) :tp) nil chan)))
  res)

(defn pmap-limited
  "Similar to pmap-full, but only runs work n-ways parallel."
  [f data n-workers]
  (assert (> n-workers 0))
  (def res (if (dictionary? data) @{} @[]))
  (var cursor (next data nil))
  (defn worker [&]
    (while (not= nil cursor)
      (def value (get data cursor))
      (def key cursor)
      (set cursor (next data cursor))
      (put res key (f value))))
  (pcall worker n-workers)
  res)

(defn pmap
  "Map `f` over data in parallel, optionally limiting parallelism to
  `n` workers."
  [f data &opt n-workers]
  (if (= nil n-workers)
    (pmap-full f data)
    (pmap-limited f data n-workers)))

(defn pdag
  "Executes a dag by calling f on every node in the graph.
  Can set the number of workers
  for parallel execution. The graph is represented as a table
  mapping nodes to arrays of child nodes. Each node will only be evaluated
  after all children have been evaluated. Modifying `dag` inside `f` 
  will not affect the scheduling of workers.
  Returns a table mapping each node
  to the result of `(f node)`."
  [f dag &opt n-workers]

  # preprocess
  (def res @{})
  (def seen @{})
  (def q (ev/chan math/int32-max))
  (def dep-counts @{})
  (def inv @{})
  (defn visit [node]
    (if (seen node) (break))
    (put seen node true)
    (def depends-on (get dag node []))
    (put dep-counts node (length depends-on))
    (if (empty? depends-on)
      (ev/give q node))
    (each r depends-on
      (put inv r (array/push (get inv r @[]) node))
      (visit r)))
  (eachk r dag (visit r))

  # run n workers in parallel
  (default n-workers (max 1 (length seen)))
  (assert (> n-workers 0))
  (defn worker [&]
    (while (next seen)
      (def node (ev/take q))
      (if-not node (break))
      (when (in seen node)
        (put seen node nil)
        (put res node (f node)))
      (each r (get inv node [])
        (when (zero? (set (dep-counts r) (dec (get dep-counts r 1))))
          (ev/give q r))))
    (ev/give q nil))

  (pcall worker n-workers)
  res)

(defn multithread-service
  "Run instances of a function over multiple threads. On failures, restart
  the failed thread. Normal function returns will not trigger a restart."
  [thread-main n-threads]
  (def supervisor (ev/thread-chan 1024))
  (var next-tid 0)
  (var to-complete n-threads)
  (defn start-thread []
    (def tid (string "thread-" (++ next-tid)))
    (ev/thread thread-main tid :nt supervisor))
  (repeat n-threads (start-thread))
  (while (> to-complete 0)
    (def [sig payload tid] (ev/take supervisor))
    (if tid
      (if (= sig :ok)
        (do
          (eprint "thread " tid " completed normally")
          (-- to-complete))
        (do
          (eprint "thread message " sig " in " tid ": " payload)
          (start-thread)))))
  (ev/chan-close supervisor))
