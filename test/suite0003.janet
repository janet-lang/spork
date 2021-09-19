(use ../spork/test)
(import ../spork/msg)

(start-suite 3)

(defn handler [s]
  (def recv (msg/make-recv s))
  (def send (msg/make-send s))
  (while (def msg (recv))
    (assert (= msg "spork") "Message 1")
    (send msg)))

(with [wt (net/server "localhost" 8000 handler)]
  (with [s (net/connect "localhost" 8000)]
    (def recv (msg/make-recv s))
    (def send (msg/make-send s))
    (send "spork")
    (assert (= (recv) "spork") "Message 2")))

(end-suite)
