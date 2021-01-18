# Helper code for running tests

(var num-tests-passed 0)
(var num-tests-run 0)
(var suite-num 0)
(var numchecks 0)
(var start-time 0)

(defn assert
  "Override's the default assert with some nice error handling."
  [x e]
  (++ num-tests-run)
  (if x (++ num-tests-passed))
  (if x
    (do
      (when (= numchecks 25)
        (set numchecks 0)
        (print))
      (++ numchecks)
      (prin "\e[32m✔\e[0m"))
    (do
      (prin "\n\e[31m✘\e[0m  ")
      (set numchecks 0)
      (print e)))
  x)

(defmacro assert-error
  "Test passes if forms error."
  [msg & forms]
  (def errsym (keyword (gensym)))
  ~(,assert (= ,errsym (try (do ,;forms) ([_] ,errsym))) ,msg))

(defmacro assert-no-error
  "Test passes if forms do not error."
  [msg & forms]
  (def errsym (keyword (gensym)))
  ~(,assert (not= ,errsym (try (do ,;forms) ([_] ,errsym))) ,msg))

(defn start-suite [x]
  "Starts test suite."
  (set suite-num x)
  (set start-time (os/clock))
  (print "\nRunning test suite " x " tests...\n"))

(defn end-suite []
  "Ends test suite."
  (def delta (- (os/clock) start-time))
  (printf "\n\nTest suite %d finished in %.3f soconds" suite-num delta)
  (print num-tests-passed " of " num-tests-run " tests passed.\n")
  (if (not= num-tests-passed num-tests-run) (os/exit 1)))
