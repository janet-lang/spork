(import spork/rawterm)

(defn on-winch
  [rows cols]
  (printf "winch - rows: %d, cols: %d" rows cols))

(print "press z to quit")

(let [input (rawterm/begin on-winch)
      buf @""]
  (defer (rawterm/end)
    (forever
      (buffer/clear buf)
      (def c (ev/read input 1 buf))
      (case (c 0)
        (chr "a") (print "Got an A an for Alan!")
        (chr "b") (print "Got a B for Bobby!")
        (chr "c") (print "Got a C for Calvin")
        (chr "z") (do (print "quitting...") (break))
        (printf "got a %V for something..." c)))))

