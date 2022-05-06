(use ../spork/test)
(import ../spork/ev-utils :as eu)

(start-suite 10)

(var x 0)
(eu/pcall (fn workerf [&] (++ x)) 10)
(assert (= x 10) "pcall 1")
(set x 0)
(eu/pcall (fn workerf [i] (+= x i)) 10)
(assert (= x 45) "pcall 2")

(end-suite)
