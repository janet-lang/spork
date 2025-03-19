# Helper code for running tests

(var num-tests-passed 0)
(var num-tests-run 0)
(var suite-num 0)
(var start-time 0)

(def- tests-passed-ref (get (dyn 'num-tests-passed) :ref))
(def- tests-run-ref (get (dyn 'num-tests-run) :ref))

(defmacro assert
  "Overrides the default assert with some nice error handling."
  [x &opt e]
  (default e (string/format "%j" (dyn :macro-form)))
  (def xx (gensym))
  ~(do
     (++ (',tests-run-ref 0))
     (def ,xx ,x)
     (if ,xx (++ (',tests-passed-ref 0)))
     (as-macro ,unless ,xx
               (if (os/isatty)
                 (,prin "\e[31m✘\e[0m  ")
                 (,prin "[FAIL] "))
               (,print ,e))
     ,xx))

(defmacro assert-not
  "Invert assert."
  [x &opt e]
  ~(as-macro ,assert (,not ,x) ,e))

(defmacro assert-error
  "Test passes if forms error."
  [msg & forms]
  (def errsym (gensym))
  ~(as-macro ,assert (,= ',errsym (as-macro ,try (do ,;forms) ([_] ',errsym))) ,msg))

(defmacro assert-no-error
  "Test passes if forms do not error."
  [msg & forms]
  (def errsym (gensym))
  ~(as-macro ,assert (,not= ',errsym (as-macro ,try (do ,;forms) ([_] ',errsym))) ,msg))

(defn start-suite
  "Starts test suite."
  [&opt name]
  (default name (dyn :current-file))
  (set suite-num name)
  (set start-time (os/clock))
  (set num-tests-passed 0)
  (set num-tests-run 0))

(defn end-suite
  "Ends test suite, prints summary and exits if any tests have failed."
  []
  (def delta (- (os/clock) start-time))
  (prinf "test suite %V finished in %.3f seconds - " suite-num delta)
  (print num-tests-passed " of " num-tests-run " tests passed.")
  (if (not= num-tests-passed num-tests-run) (os/exit 1)))

(defmacro timeit
  ```
  Time the execution of `form` using `os/clock` before and after,
  and print the result to stdout.  Returns result of executing `form`.
  Uses `tag` (default "Elapsed time:") to tag the printout.
  ```
  [form &opt tag]
  (default tag "Elapsed time:")
  (with-syms [start result end]
    ~(do
       (def ,start (os/clock))
       (def ,result ,form)
       (def ,end (os/clock))
       (print ,tag " " (- ,end ,start) " seconds")
       ,result)))

(defmacro timeit-loop
  ``Similar to `loop`, but outputs performance statistics after completion.
  Additionally defines a `:timeout` verb to iterate continuously for a given
  number of seconds. If the first form of `body` is a bytes, it will be taken
  as a custom tag.``
  [head & body]
  (var tag "Elapsed time:")
  (def head2 @[;head])
  (def body2 @[;body])
  (with-syms [c start elapsed per-body]
    (when (def i (index-of :timeout head2))
      (array/insert head2 i [])
      (set (head2 (+ i 1)) :iterate)
      (set (head2 (+ i 2)) ~(< (- (os/clock) ,start) ,(in head2 (+ i 2)))))
    (when (bytes? (get body2 0))
      (set tag (in body2 0))
      (array/remove body2 0))
    ~(do
       (var ,c 0)
       (def ,start (os/clock))
       (loop ,head2 (++ ,c) ,;body2)
       (def ,elapsed (- (os/clock) ,start))
       (def ,per-body (/ ,elapsed ,c))
       (cond
         (< ,per-body 1e-3) (printf "%s %.3fs, %.4gµs/body" ,tag ,elapsed (* ,per-body 1_000_000))
         (< ,per-body 1) (printf "%s %.3fs, %.4gms/body" ,tag ,elapsed (* ,per-body 1_000))
         (printf "%s %.3fs, %.4gs/body" ,tag ,elapsed ,per-body)))))

(defmacro- capture-*
  [out & body]
  (with-syms [buf res]
    ~(do
       (def ,buf @"")
       (with-dyns [,out ,buf]
         (def ,res (do ,;body))
         [,res (string ,buf)]))))

(defmacro capture-stdout
  ```
  Runs the form and captures stdout. Returns tuple with result of the form
  and a string with captured stdout.
  ```
  [& body]
  ~(as-macro ,capture-* :out ,;body))

(defmacro capture-stderr
  ```
  Runs the form and captures stderr. Returns tuple with result of the form
  and a string with captured stderr.
  ```
  [& body]
  ~(as-macro ,capture-* :err ,;body))

(defmacro- suppress-* [out & body]
  ~(with-dyns [,out @""] ,;body))

(defmacro suppress-stdout
  "Suppreses stdout from the body"
  [& body]
  ~(as-macro ,suppress-* :out ,;body))

(defmacro suppress-stderr
  "Suppreses stderr from the body"
  [& body]
  ~(as-macro ,suppress-* :err ,;body))

(defn assert-docs
  ```
  Assert that all symbols have proper docstring when module on the
  path is required.
  ```
  [path]
  (loop [[sym val] :pairs (require path)
         :when (and (symbol? sym) (not (val :private)) (not (val :ref)))]
    (assert (and (val :doc)
                 (peg/match '(* (+ (* "(" (thru ")\n\n"))
                                   (not "("))
                                (some 1) -1)
                            (get val :doc "")))
            (string sym " does not have proper doc"))))
