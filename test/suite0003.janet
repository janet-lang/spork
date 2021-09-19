(use ../spork/test)
(import ../spork/msg)

(start-suite 3)

(defn handler [s]
  (def recv (msg/make-recv s))
  (def send (msg/make-send s))
  (while (def msg (recv))
    (assert (= msg "spork") "Message 1")
    (send msg)))

(defn wrk [m] (net/server "localhost" 8000 handler))

(def wt (thread/new wrk 1 :h)) # we need heavy thread for the assert

(os/sleep 0.1) # give server thread little time to start

(defer (:close wt)
  (def s (net/connect "localhost" 8000))
  (def recv (msg/make-recv s))
  (def send (msg/make-send s))
  (send "spork")
  (assert (= (recv) "spork") "Message 2"))
(end-suite)
