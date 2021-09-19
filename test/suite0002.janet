(use ../spork/test)
(import ../spork/rpc)

(start-suite 2)

(def fns
  {:hi (fn [self msg]
         (assert (= msg "spork") "RPC server")
         (string "Hello " msg))})

(defn wrk [m] (rpc/server fns "localhost" 8000))

(def wt (thread/new wrk 1 :h)) # so need heavy thread for the assert

(os/sleep 0.1) # give server thread little time to start

(defer (:close wt)
  (def c (rpc/client "localhost" 8000))
  (assert (= (:hi c "spork") "Hello spork") "RPC client")
  (:close c))

(end-suite)
