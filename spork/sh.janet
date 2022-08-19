(defn exec-slurp
   "Read stdout of subprocess and return it trimmed in a string."
   [& args]
   (when (dyn :verbose)
     (flush)
     (print "(exec-slurp " ;(interpose " " args) ")"))
   (def proc (os/spawn args :px {:out :pipe}))
   (def out (get proc :out))
   (def buf @"")
   (ev/gather
     (:read out :all buf)
     (:wait proc))
   (string/trimr buf))
