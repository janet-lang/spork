###
### rpc.janet
###
### Simple RPC server and client tailored to Janet.
###

### Limitations:
###
### Currently calls are resolved in the order that they are sent
### on the connection - in other words, a single RPC server must resolve
### remote calls sequentially. This means it is recommended to make multiple
### connections for separate transactions.

(use ./msg)

(def default-host
  "Default host to run server on and connect to."
  "127.0.0.1")

(def default-port
  "Default port to run the net repl."
  "9366")

# RPC Protocol
#
# 1. server <- {user specified name of client} <- client
# 2. server -> {marshalled tuple of supported keys in marshal dict (capabilites)} -> client
# 3. server <- {marshalled function call: [fnname args]} <- client
# 4. server -> {result of unmarshalled call: [status result]} -> client
# 5. go back to 3.

(defn server
  "Create an RPC server. The default host is \"127.0.0.1\" and the
  default port is \"9366\". Also must take a dictionary of functions
  that clients can call."
  [functions &opt host port]
  (default host default-host)
  (default port default-port)
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
        (while (def msg (recv))
          (try
            (let [[fnname args] msg
                  f (functions fnname)]
              (if-not f
                (error (string "no function " fnname " supported")))
              (def result (f functions ;args))
              (send [true result]))
            ([err]
              (send [false err]))))))))

(defn client
  "Create an RPC client. The default host is \"127.0.0.1\" and the
  default port is \"9366\". Returns a table of async functions
  that can be used to make remote calls. This table also contains
  a close function that can be used to close the connection."
  [&opt host port name]
  (default host default-host)
  (default port default-port)
  (default name (string "[" host ":" port "]"))
  (def stream (net/connect host port))
  (def recv (make-recv stream unmarshal))
  (def send (make-send stream marshal))
  (send name)
  (def fnames (recv))
  (defn closer [&] (:close stream))
  (def ret @{:close closer})
  (each f fnames
    (put ret (keyword f)
         (fn rpc-function [_ & args]
           (send [f args])
           (let [[ok x] (recv)]
             (if ok x (error x))))))
  ret)
