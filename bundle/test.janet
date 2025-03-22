#
# Run test scripts, and error if any fail.
# Pass through any args
#

(defn main [& argv]
  (def failures-array @[])
  (each suite (sorted (os/dir "test"))
    (when (string/has-prefix? "suite-" suite)
      (eprint "Running suite " suite)
      (def result (os/execute [(dyn *executable*) "-m" (os/realpath (dyn *syspath*)) (string "test/" suite) ;argv] :p))
      (if-not (zero? result) 
        (array/push failures-array suite))))
  (if (empty? failures-array)
    (eprint "All Passed.")
    (errorf "Failures in: %s" (string/join failures-array ", "))))
