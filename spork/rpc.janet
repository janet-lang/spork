###
### rpc.janet
###
### Simple RPC server and client tailored to Janet.
###

(use ./msg)
(use ./ev-utils)

(def default-host
  "Default host to run server on and connect to."
  "127.0.0.1")

(def default-port
  "Default port to run the net repl."
  "9366")

# RPC Protocol
#
# All prodcedure calls must be accompanied by a nonce value. This can be any non-repeating
# value that is unique per-connection - a sequence of increasing integers works well.
#
# 1. server <- {user specified name of client} <- client
# 2. server -> {marshalled tuple of supported keys in marshal dict (capabilites)} -> client
# 3. server <- {marshalled function call: [nonce fnname args]} <- client
# 4. server -> {result of unmarshalled call: [nonce status result]} -> client
# 5. go back to 3.

(defn server
  "Create an RPC server. The default host is \"127.0.0.1\" and the
  default port is \"9366\". Also must take a dictionary of functions
  that clients can call."
  [functions &opt host port workers-per-connection]
  (default host default-host)
  (default port default-port)
  (default workers-per-connection 1)
  (def keys-msg (keys functions))
  (net/server
    host port
    (fn on-connection
      [stream]
      (var name "<unknown>")
      (def marshbuf @"")
      (defer (:close stream)
        (def recv (make-recv stream unmarshal))
        (def send (make-send stream marshal))
        (set name (or (recv) (break)))
        (send keys-msg)
        (def chan-size (* 8 workers-per-connection))
        (def in-queue (ev/chan chan-size))
        (def out-queue (ev/chan chan-size))
        (def n (nursery))

        # Spawn message consumer - the only fiber reading from socket
        (spawn-nursery
          n
          (protect
            (while (def msg (recv))
              (ev/give in-queue msg)))
          (ev/chan-close out-queue)
          (ev/chan-close in-queue))

        # Spawn message producer - the only fiber writing to the socket
        (spawn-nursery
          n
          (while (def msg (ev/take out-queue))
            (when (= :close (get msg 0)) (break)) # handle closed channel
            (send msg)))

        # Spawn n workers
        (repeat
          workers-per-connection
          (spawn-nursery
            n
            (while (def msg (ev/take in-queue))
              (def [id name args] msg)
              (when (= :close id) (break))
              (try
                (let [f (functions name)]
                  (if-not f
                    (error (string "no function " name " supported")))
                  (def result (f functions ;args))
                  (ev/give out-queue [id true result]))
                ([err]
                  (ev/give out-queue [id false err]))))))

        # Wait for fibers to finish
        (join-nursery n)))))

(defn client
  "Create an RPC client. The default host is \"127.0.0.1\" and the
  default port is \"9366\". Returns a table of async functions
  that can be used to make remote calls. This table also contains
  a :close function that can be used to close the connection."
  [&opt host port name]
  (default host default-host)
  (default port default-port)
  (default name (string "[" host ":" port "]"))
  (def stream (net/connect host port))
  (def recv (make-recv stream unmarshal))
  (def send (make-send stream marshal))

  # Get methods
  (send name)
  (def fnames (recv))

  # Build table
  (def chans @{})
  (def send-chan (ev/chan))
  (defn closer [&] (:close stream) (ev/chan-close send-chan))
  (var nonce 0)

  (def producer
    (ev/spawn
      (while (def msg (ev/take send-chan))
        (if (= :close (get msg 0)) (break))
        (send msg))))

  (def consumer
    (ev/spawn
      (while (def msg (recv))
        (def [id] msg)
        (def c (get chans id))
        (when c
          (put chans id nil)
          (ev/give c msg)))))

  # Use prototype to isolate rpc methods but still leave
  # access to internal fields - references to things like :close
  # can be overwritten but not deleted by rpc methods.
  (def ret
    (table/setproto
      @{}
      @{:close closer
        :channels chans
        :send-chan send-chan
        :consumer consumer
        :producer producer}))

  # Add methods from server
  (each f fnames
    (def k (keyword f))
    (put ret k
         (fn rpc-function [_ & args]
           (def id (++ nonce))
           (def c (ev/chan 1))
           (put chans id c)
           (ev/give send-chan [id f args])
           (let [[_ ok x] (ev/take c)]
             (if ok x (error x))))))

  ret)
