(use ../spork/test)
(import ../spork/rpc)

(start-suite 2)

(def fns
  {:hi (fn [self msg]
         (string "Hello " msg))})

(with [wt (rpc/server fns "localhost" 8000)]
  (with [c (rpc/client "localhost" 8000)]
    (assert (= (:hi c "spork") "Hello spork") "RPC client")
    # parallel
    (ev/gather
      (assert (= (:hi c 0) (string "Hello " 0)) "RPC client parallel")
      (assert (= (:hi c 1) (string "Hello " 1)) "RPC client parallel")
      (assert (= (:hi c 2) (string "Hello " 2)) "RPC client parallel")
      (assert (= (:hi c 3) (string "Hello " 3)) "RPC client parallel"))))

(end-suite)
