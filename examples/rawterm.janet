(import spork/rawterm)

(defn on-winch
  [rows cols]
  (printf "winch - rows: %d, cols: %d" rows cols))

(print "press z to quit")

(defer (rawterm/end)
  (rawterm/begin on-winch)
  (forever
    (def [c] (rawterm/getch))
    (case c
      (chr "a") (print "Got an A an for Alan!")
      (chr "b") (print "Got a B for Bobby!")
      (chr "c") (print "Got a C for Calvin")
      (chr "z") (do (print "quitting...") (break))
      (printf "got a %c for something..." c))))
