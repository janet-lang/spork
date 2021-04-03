###
### term-report.janet
###
### Module for generated pseudo-grpahical reports from a monospaced font.
### Prints the graphics to (dyn :out).
###

(defn- prin-lpad
  [str n]
  (prin (string/repeat " " (- n (length str))) str))

(defn- prin-rpad
  [str n]
  (prin str (string/repeat " " (- n (length str)))))

(defn print-cols
  "Print a report table to (dyn :out). The table data is expected to be a
  dictionary mapping headers to data columns. Returns the number of lines printed."
  [data &opt headers]
  (default headers (sort (keys data)))

  # Short circuit empty table
  (when (empty? headers)
    (print)
    (break))

  # Convert data to strings, and find column widths
  (def strdata @{})
  (def widths @{})
  (var numrow 0)
  (def buf-led @"┌")
  (def buf-sep @"╞")
  (def buf-end @"└")
  (each h headers
    (def strcol (map string (in data h)))
    (put strdata h strcol)
    (var w (length h))
    (each x strcol
      (def lx (length x))
      (if (> lx w) (set w lx)))
    (def lcol (length strcol))
    (if (> lcol numrow) (set numrow lcol))
    (put widths h w)
    (when (not= h (first headers))
      (buffer/push buf-led "┬")
      (buffer/push buf-sep "╪")
      (buffer/push buf-end "┴"))
    (def span (string/repeat "─" w))
    (def span-sep (string/repeat "═" w))
    (buffer/push buf-led span)
    (buffer/push buf-sep span-sep)
    (buffer/push buf-end span))
  (buffer/push buf-led "┐")
  (buffer/push buf-sep "╡")
  (buffer/push buf-end "┘")

  # Print headers
  (print buf-led)
  (each h headers
    (prin "│")
    (prin-rpad h (in widths h)))
  (print "│")
  (print buf-sep)
  (for i 0 numrow
    (each h headers
      (prin "│")
      (prin-lpad (get (in strdata h) i "") (in widths h)))
    (print "│"))
  (print buf-end)
  (+ 4 numrow))

(defn print-tree
  "Show dependencies for a given rule recursively in a nice tree.
  Takes a root node to be printed, as well as a function children used to get
  child notes to print. A leaf node should return an empty set of children."
  [root children &opt depth prefix prefix-part]
  (default depth math/inf)
  (print prefix root)
  (when-let [kids (children root)]
    (when (pos? depth)
      (def l (-> kids length dec))
      (eachp [i d] kids
        (print-tree
          d children (dec depth)
          (string prefix-part (if (= i l) " └─" " ├─"))
          (string prefix-part (if (= i l) "   " " │ ")))))))


