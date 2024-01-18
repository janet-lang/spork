###
### A Janet implementation of `(getline)` that is non-blocking.
### Allows for beter integration in netrepl (completions and docs can be streamed
### over the network).
###

### TODO
# - unit testing?

(import spork/rawterm)

(def max-history "Maximal amount of items in the history" 500)

(def- sym-prefix-peg
  (peg/compile
    ~{:symchar (+ (range "\x80\xff" "AZ" "az" "09") (set "!$%&*+-./:<?=>@^_"))
      :anchor (drop (cmt ($) ,|(= $ 0)))
      :cap (* (+ (> -1 (not :symchar)) :anchor) (* ($) '(some :symchar)))
      :recur (+ :cap (> -1 :recur))
      :main (> -1 :recur)}))

(defn default-autocomplete-context
  "Given a buffer and a cursor position, extract a string that will be used as context for autocompletion.
  Return a position and substring from the buffer to use for autocompletion."
  [buf pos]
  (peg/match sym-prefix-peg buf pos))

(defn- greatest-common-prefix
  "Find the greatest common prefix for autocompletion"
  [a b]
  (if (> (length a) (length b))
    (greatest-common-prefix b a)
    (slice a 0 (length (take-until |(not $) (map = a b))))))

(defn default-doc-fetch
  "Default handler for Ctrl-G to lookup docstrings in the current environment."
  [sym w &]
  (def doc-entry (get root-env (symbol sym)))
  (when doc-entry
    (def doc-string (get doc-entry :doc))
    (when doc-string
      (string "\n" (doc-format doc-string w 4 true)))))

(defn default-autocomplete-options
  "Default handler to get available autocomplete options for a given substring."
  [prefix &]
  (def seen @{})
  (def ret @[])
  (var env (curenv))
  (while env
    (eachk symname env
      (when (not (seen symname))
        (when (symbol? symname)
          (when (string/has-prefix? prefix symname)
            (put seen symname true)
            (array/push ret symname)))))
    (set env (table/getproto env)))
  (sort ret)
  ret)

(defn make-getline
  "Reads a line of input into a buffer, like `getline`. However, allow looking up entries with a general
  lookup function rather than an environment table."
  [&opt autocomplete-context autocomplete-options doc-fetch]

  (default autocomplete-context default-autocomplete-context)
  (default autocomplete-options default-autocomplete-options)
  (default doc-fetch default-doc-fetch)

  # state
  (var w "Last measured terminal width (columns)" 0)
  (var h "Last measured height (rows)" 0)
  (var buf "Line buffer" @"")
  (var prpt "Prompt string (line prefix)" "")
  (var prpt-width "Prompt string character width" 0)
  (def history "History stack. Top item is current placeholder." @[])
  (def tmp-buf "Buffer to group writes to stderr for terminal rendering." @"")
  (var pos "Cursor byte position in buf. Must be on valid utf-8 start byte at all times." 0)
  (var lines-below "Number of dirty lines below input line for drawing cleanup." 0)
  (var ret-value "Value to return to caller, usually the mutated buffer." buf)
  (var more-input "Loop condition variable" true)
  (def input-buf @"")

  (defn getc
    "Get next character. Caller needs to check input buf after call if utf8 sequence returned (>= c 0x80)"
    []
    (buffer/clear input-buf)
    (rawterm/getch input-buf)
    (def c (get input-buf 0))
    (when (>= c 0x80)
      (repeat (cond
                (= (band c 0xF8) 0xF0) 3
                (= (band c 0xF0) 0xE0) 2
                1)
        (rawterm/getch input-buf)))
    c)

  (defn- flushs
    []
    (eprin tmp-buf)
    (eflush)
    (buffer/clear tmp-buf))

  (defn- clear
    []
    (eprin "\e[H\e[2J")
    (eflush))

  (defn- clear-lines
    []
    (repeat lines-below
      (buffer/push tmp-buf "\e[1B\e[999D\e[K"))
    (when (pos? lines-below)
      (buffer/format tmp-buf "\e[%dA\e[999D" lines-below)
      (set lines-below 0))
    (flushs))

  (defn- refresh
    []
    (def available-w (- w prpt-width))
    (def at-end (= pos (length buf)))
    (def next-pos (rawterm/buffer-traverse buf pos 1 true))
    (def width-under-cursor (if at-end 1 (rawterm/monowidth buf pos next-pos)))
    (var columns-to-pos (+ width-under-cursor (rawterm/monowidth buf 0 pos)))
    (def shift-right-amnt (max 0 (- columns-to-pos available-w)))

    # Strange logic to handle the case when the windowing code would cut a character in half.
    (def test-buf (rawterm/slice-monowidth buf shift-right-amnt))
    (def add-space-padding (not= shift-right-amnt (rawterm/monowidth test-buf)))
    (def view-pos
      (if add-space-padding
        (rawterm/buffer-traverse buf (length test-buf) 1 true)
        (length test-buf)))
    (def pad (if add-space-padding " " ""))

    (def view (rawterm/slice-monowidth buf available-w view-pos))
    (def visual-pos (+ prpt-width (- columns-to-pos shift-right-amnt width-under-cursor)))
    (buffer/format tmp-buf "\r%s%s%s\e[0K\r\e[%dC" prpt pad view visual-pos)
    (flushs))

  (defn- history-move
    [idx delta]
    (def new-idx (min (dec (length history)) (max 0 (+ idx delta))))
    (when (not= idx new-idx)
      (when (= idx (dec (length history)))
        (put history idx (string buf)))
      (buffer/clear buf)
      (buffer/push buf (in history new-idx))
      (set pos (length buf))
      (refresh))
    new-idx)

  (defn- check-overflow
    []
    (def available-w (- w prpt-width))
    (- (rawterm/monowidth buf) available-w))

  (defn- insert
    [bytes &opt draw]
    (buffer/push buf bytes)
    (def old-pos pos)
    (+= pos (length bytes))
    (if (= (length buf) pos)
      (do
        (when draw
          (def o (check-overflow))
          (if (>= o 0)
            (refresh)
            (do
              (buffer/push tmp-buf bytes)
              (flushs)))))
      (do
        (buffer/blit buf buf pos old-pos -1)
        (buffer/blit buf bytes old-pos)
        (buffer/popn buf (length bytes))
        (if draw (refresh)))))

  (defn- autocomplete
    []
    (unless autocomplete-options (break))
    (def ctx (autocomplete-context buf pos))
    (unless ctx (break))
    (def [ctx-pos ctx-string] ctx)
    (set pos (+ ctx-pos (length ctx-string)))
    (def options (autocomplete-options ctx-string buf pos))
    (clear-lines)
    (case (length options)
      0 (refresh)
      1
      (do
        (def choice (get options 0))
        (insert (string/slice choice (length ctx-string)))
        (refresh))
      (do # print all options
        (def gcp (reduce greatest-common-prefix (first options) options))
        (insert (string/slice gcp (length ctx-string)))
        (def maxlen (extreme > (map length options)))
        (def colwidth (+ 4 maxlen))
        (def cols (max 1 (math/floor (/ w colwidth))))
        (def rows (partition cols options))
        (def padding (string/repeat " " colwidth))
        (set lines-below (length rows))
        (each row rows
          (eprint)
          (each item row
            (eprin (slice (string item padding) 0 colwidth))))
        (eprinf "\e[%dA" lines-below)
        (eflush)
        (refresh))))

  (defn- showdoc
    []
    (unless doc-fetch (break))
    (def ctx (autocomplete-context buf pos))
    (unless ctx (break))
    (def [_ ctx-string] ctx)
    (def doc-source (doc-fetch ctx-string w h))
    (unless doc-source (break))
    (clear-lines)
    (set lines-below (length (string/find-all "\n" doc-source)))
    (eprin doc-source)
    (eprinf "\e[%dA" lines-below)
    (refresh))

  (defn- kleft
    [&opt draw]
    (default draw true)
    (def new-pos (rawterm/buffer-traverse buf pos -1 true))
    (when new-pos
      (set pos new-pos)
      (if draw (refresh))))

  (defn- kleftw
    []
    (while (and (> pos 0) (= 32 (buf (dec pos)))) (kleft false))
    (while (and (> pos 0) (not= 32 (buf (dec pos)))) (kleft false))
    (refresh))

  (defn- kright
    [&opt draw]
    (default draw true)
    (def new-pos (or (rawterm/buffer-traverse buf pos 1 true) (length buf)))
    (set pos new-pos)
    (if draw (refresh)))

  (defn- krightw
    []
    (while (and (< pos (length buf)) (not= 32 (buf pos))) (kright false))
    (while (and (< pos (length buf)) (= 32 (buf pos))) (kright false))
    (refresh))

  (defn- khome
    []
    (set pos 0)
    (refresh))

  (defn- kend
    []
    (set pos (length buf))
    (refresh))

  (defn- kback
    [&opt draw]
    (default draw true)
    (def new-pos (rawterm/buffer-traverse buf pos -1 true))
    (when new-pos
      (buffer/blit buf buf new-pos pos)
      (buffer/popn buf (- pos new-pos))
      (set pos new-pos)
      (if draw (refresh))))

  (defn- kbackw
    []
    (while (and (> pos 0) (= 32 (buf (dec pos)))) (kback false))
    (while (and (> pos 0) (not= 32 (buf (dec pos)))) (kback false))
    (refresh))

  (defn- kdelete
    [&opt draw]
    (default draw true)
    (kright false)
    (kback draw))

  (defn- kdeletew
    []
    (while (and (< pos (length buf)) (= 32 (buf pos))) (kdelete false))
    (while (and (< pos (length buf)) (not= 32 (buf pos))) (kdelete false))
    (refresh))

  (fn getline-fn
    [&opt prompt buff _]
    (set buf (or buff @""))
    (set prpt (string prompt))
    (set prpt-width (rawterm/monowidth prpt))
    (unless (rawterm/isatty)
      (break (getline prpt buf)))
    (defer (rawterm/end)
      (rawterm/begin)
      (buffer/clear tmp-buf)
      (buffer/clear buf)
      (set ret-value buf)
      (set pos 0)
      (set lines-below 0)
      (set more-input true)
      (eprin prpt)
      (eflush)
      (array/push history "")
      (if (> (length history) max-history) (array/remove history 0))
      (var hindex (dec (length history)))
      (while more-input
        (def c (getc))
        (def [_h _w] (rawterm/size))
        (set w _w)
        (set h _h)
        (if (>= c 0x20)
          (case c
            127 (kback)
            (when (>= c 0x20)
              (insert input-buf true)))
          (case c
            1 # ctrl-a
            (khome)
            2 # ctrl-b
            (kleft)
            3 # ctrl-c
            (do (clear-lines) (eprint "^C") (eflush) (rawterm/end) (os/exit 1))
            4 # ctrl-d, eof
            (if (= pos (length buf) 0)
              (do (set more-input false) (clear-lines))
              (kdelete))
            5 # ctrl-e
            (kend)
            6 # ctrl-f
            (kright)
            7 # ctrl-g
            (showdoc)
            8 # ctrl-h
            (kback)
            9 # tab
            (autocomplete)
            11 # ctrl-k
            (do (buffer/popn buf (- (length buf) pos)) (refresh))
            12 # ctrl-l
            (do (clear) (refresh))
            13 # enter
            (do (set more-input false) (buffer/push buf "\n") (clear-lines))
            14 # ctrl-n
            (set hindex (history-move hindex -1))
            16 # ctrl-p
            (set hindex (history-move hindex 1))
            17 # ctrl-q
            (do (set more-input false) (set ret-value :cancel) (clear-lines))
            23 # ctrl-w
            (kbackw)
            26 # ctrl-z
            (do (rawterm/ctrl-z) (refresh))
            27 # escape sequence, process more
            (case (getc)
              (chr "[")
              (let [c3 (getc)]
                (cond
                  (and (>= c3 (chr "0")) (<= c3 (chr "9")))
                  (case (def c4 (getc))
                    (chr "1") (khome)
                    (chr "3") (kdelete)
                    (chr "4") (kend)
                    126 (kdelete))
                  (= c3 (chr "O"))
                  (case (getc)
                    (chr "H") (khome)
                    (chr "F") (kend))
                  (= c3 (chr "A")) (set hindex (history-move hindex -1))
                  (= c3 (chr "B")) (set hindex (history-move hindex 1))
                  (= c3 (chr "C")) (kright)
                  (= c3 (chr "D")) (kleft)
                  (= c3 (chr "H")) (khome)
                  (= c3 (chr "F")) (kend)))
              (chr "d") (kdeletew) # alt-d
              (chr "b") (kleftw) # alt-b
              (chr "f") (krightw) # alt-f
              (chr ",") (set hindex (history-move hindex (- max-history)))
              (chr ".") (set hindex (history-move hindex max-history))
              127 (kbackw)
              nil))))
      (eprint)
      (if (= buf ret-value)
        (if (= "" (string/trimr buf))
          (array/pop history)
          (put history (dec (length history)) (string/trimr buf "\n")))
        (array/pop history))
      ret-value)))
