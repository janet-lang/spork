(use ../spork/test)
(use ../spork/sh-dsl)
(import ../spork/sh :as sh)

(start-suite)

(def janet (sh/self-exe))

# TODO - windows has a very different command line environment
(def is-windows (= :windows (os/which)))

# Basic commands
(assert ($ echo) "echo 1")
(assert (deep= @"hi\n" ($< echo hi)) "echo 2")
(assert (deep= @"hi" ($<_ echo hi)) "echo 3")

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

(end-suite)
