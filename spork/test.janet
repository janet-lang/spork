# Helper code for running tests

(var num-tests-passed 0)
(var num-tests-run 0)
(var suite-num 0)
(var start-time 0)

(defn assert
  "Override's the default assert with some nice error handling."
  [x &opt e]
  (default e "assert error")
  (++ num-tests-run)
  (if x (++ num-tests-passed))
  (unless x
    (prin "\e[31m✘\e[0m  ")
    (print e))
  x)

(defn assert-not
  "Invert assert."
  [x &opt e]
  (assert (not x) e))

(defmacro assert-error
  "Test passes if forms error."
  [msg & forms]
  (def errsym (gensym))
  ~(,assert (= ',errsym (try (do ,;forms) ([_] ',errsym))) ,msg))

(defmacro assert-no-error
  "Test passes if forms do not error."
  [msg & forms]
  (def errsym (gensym))
  ~(,assert (not= ',errsym (try (do ,;forms) ([_] ',errsym))) ,msg))

(defn start-suite
  "Starts test suite."
  [&opt name]
  (default name (dyn :current-file))
  (set suite-num name)
  (set start-time (os/clock))
  (set num-tests-passed 0)
  (set num-tests-run 0))

(defn end-suite
  "Ends test suite, prints summary and exits if any have failed."
  []
  (def delta (- (os/clock) start-time))
  (prinf "test suite %V finished in %.3f seconds - " suite-num delta)
  (print num-tests-passed " of " num-tests-run " tests passed.")
  (if (not= num-tests-passed num-tests-run) (os/exit 1)))

(defmacro timeit
  ```
  Time the execution of `form` using `os/clock` before and after,
  and print the result to stdout.  returns: result of executing `form`.
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
  "Suppreses stdout from the body"
  [& body]
  ~(as-macro ,suppress-* :err ,;body))

(defn assert-docs
  ```
  Assert that all symbols, when module on the path is required,
  have proper doc string
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
