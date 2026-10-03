(use spork/test)
(import spork/rpc)

(start-suite)

(def fns
  {:hi (fn [self msg]
         (string "Hello " msg))})

# required because (rpc/server functions "localhost" ...) may bind to ::1 or
# 127.0.0.1 on systems with both IP stacks. You don't know which it will be.
(defn client-by-name
    ``
    Like rpc/client, but try connecting to each address getaddrinfo(3)
    returns for `host` until one succeeds. Raise error if none succeed.
    ``
    [host &opt port name]
    (default port rpc/default-port)
    (var err nil)
    (var client nil)

    (let [optional   (filter truthy? [port name])
          sock-addrs (net/address host port :stream true)
          addrs      (map |(first (net/address-unpack $)) sock-addrs)]
      (loop [addr :in addrs :while (nil? client)]
        (try
          (set client (rpc/client addr ;optional))
          ([e] (set err e)))))

    # `net/address` will raise if DNS failed, so we either have client or err
    (if (nil? client) err client))

(with [wt (rpc/server fns "localhost" 8000)]
  (with [c (client-by-name "localhost" 8000)]
    (assert (= (:hi c "spork") "Hello spork") "RPC client")
    # parallel
    (ev/gather
      (assert (= (:hi c 0) (string "Hello " 0)) "RPC client parallel")
      (assert (= (:hi c 1) (string "Hello " 1)) "RPC client parallel")
      (assert (= (:hi c 2) (string "Hello " 2)) "RPC client parallel")
      (assert (= (:hi c 3) (string "Hello " 3)) "RPC client parallel"))))

(end-suite)
