(use spork/test)
(import spork/msg)

(start-suite)

(defn handler [s]
  (def recv (msg/make-recv s))
  (def send (msg/make-send s))
  (while (def msg (recv))
    (assert (= msg "spork") "Message 1")
    (send msg)))

# required because (net/server "localhost" ...) may bind to ::1 or 127.0.0.1
# on systems with both IP stacks. You don't know which it will be.
(defn connect-by-name
    ``
    Like net/connect, but try connecting to each address getaddrinfo(3)
    returns for `host` until one succeeds. Raise error if none succeed.
    ``
    [host port &opt type bindhost bindport]
    (default type :stream)
    (var err nil)
    (var socket nil)

    (let [optional   (filter truthy? [type bindhost bindport])
          sock-addrs (net/address host port type true)
          addrs      (map |(first (net/address-unpack $)) sock-addrs)]
      (loop [addr :in addrs :while (nil? socket)]
        (try
          (set socket (net/connect addr port ;optional))
          ([e] (set err e)))))

    # `net/address` will raise if DNS failed, so we either have socket or err
    (if (nil? socket) err socket))

(with [wt (net/server "localhost" 8000 handler)]
  (with [s (connect-by-name "localhost" 8000)]
    (def recv (msg/make-recv s))
    (def send (msg/make-send s))
    (send "spork")
    (assert (= (recv) "spork") "Message 2")))

(end-suite)
