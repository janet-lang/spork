(use ../spork/test)
(import ../spork/rpc)

(start-suite 2)

(def fns
  {:hi (fn [self msg]
         (assert (= msg "spork") "RPC server")
         (string "Hello " msg))})

(with [wt (rpc/server fns "localhost" 8000)]
  (with [c (rpc/client "localhost" 8000)]
    (assert (= (:hi c "spork") "Hello spork") "RPC client")))

(end-suite)
