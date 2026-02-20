(use ../spork/test)
(use ../spork/sh-dsl)
(import ../spork/sh :as sh)

(start-suite)

(assert true)
(def janet (sh/self-exe))

# TODO - windows has a very different command line environment
(def is-windows (= :windows (os/which)))

# Basic commands
(unless is-windows
  (assert ($< echo) "echo 1")
  (assert (deep= @"hi\n" ($< echo hi)) "echo 2")
  (assert (deep= @"hi" ($<_ echo hi)) "echo 3")

  # Malformed (eval wrapped since these are macros)
  (assert-error "malformed 1" (eval '($)))
  (assert-error "malformed 2" (eval '($<)))
  (assert-error "malformed 3" (eval '($<_)))
  (assert-error "malformed 4" (eval '($< < README.md)))

  # Pipes
  (assert (deep= @"hi" ($<_ echo "hi\nhello" | grep hi)) "pipeline 1")
  (assert-error "bad program" (deep= @"hi" ($<_ echo "hi\nhello" | greppy-123abc456789 hi)))
  (assert (deep= @"1" ($<_ ,janet -e "(print 1)" | grep 1)) "janet shell-out 1")
  (assert (deep= @"1" ($<_ ,janet -e "(print 1)" | grep 1 | grep 1 | grep 1 | grep 1 | grep 1 | grep 1)) "long pipeline 1")

  # Environment variables
  (assert ($? BONKERS=1 ,janet -e "(assert (os/getenv `BONKERS`))") "setting env vars works 1")
  (assert (not ($? ,janet -e "(assert (os/getenv `BONKERS`))" :err-to-out > ,(sh/devnull))) "setting env vars works 2")
  (os/setenv "BONKERS" "2")
  (assert (deep= @"2" ($<_ echo $BONKERS)) "passing env vars works 1")
  (assert (deep= @"BONKERS=2" ($<_ echo BONKERS=2)) "setting env vars doesn't work after program name")

  # More pipes using just janet
  (def version (buffer janet/version "-" janet/build))
  (assert (deep= version ($<_ ,janet --version | ,janet -e '(prin (:read stdin :all)))) "janet pipe example")

  # Pipefail
  (setdyn *pipefail* true)
  (assert ($< echo) "echo pipefail 1")
  (assert (deep= @"hi\n" ($< echo hi)) "echo pipefail 2")
  (assert (deep= @"hi" ($<_ echo hi)) "echo pipefail 3")
  (assert (= 1 ($< ,janet -e "(os/exit 0)" | ,janet -e "(os/exit 1)" | ,janet -e "(os/exit 0)")) "pipefail 1")

  # Errexit
  (setdyn *pipefail* false)
  (setdyn *errexit* true)
  (assert ($< echo) "echo errexit 1")
  (assert (deep= @"hi\n" ($< echo hi)) "echo errexit 2")
  (assert (deep= @"hi" ($<_ echo hi)) "echo errexit 3")
  (assert-error "errexit grep" ($ echo hi | grep nope))
  (assert ($? ,janet -e "(os/exit 0)") "$? works normally with errexit 1")
  (assert (not ($? ,janet -e "(os/exit 1)")) "$? works normally with errexit 2")

  # Pipefail and Errexit
  (setdyn *pipefail* true)
  (assert ($< echo) "echo pipefail + errexit 1")
  (assert (deep= @"hi\n" ($< echo hi)) "echo pipefail + errexit 2")
  (assert (deep= @"hi" ($<_ echo hi)) "echo pipefail + errexit 3")
  (assert-error "pipefail + errexit grep" ($ echo hi | grep nope))
  (assert-error "pipefail + errexit grep" ($ echo hi | grep nope | ,janet -e "(os/exit 0)")))

(end-suite)
