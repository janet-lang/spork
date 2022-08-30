###
### A Janet implementation of `(getline)` that is non-blocking.
### Allows for beter integration in netrepl (completions and docs can be streamed
### over the network).
###

### TODO
# - unit testing?
# - character width detection for better utf-8 support

(import spork/rawterm)

(def max-history 500)

(def- sym-prefix-peg
  (peg/compile
    ~{:symchar (+ (range "\x80\xff" "az" "az" "09") (set "!$%&*+-./:<?=>@^_"))
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
  lookup function rather than a environment table."
  [&opt autocomplete-context autocomplete-options doc-fetch]

  (default autocomplete-context default-autocomplete-context)
  (default autocomplete-options default-autocomplete-options)
  (default doc-fetch default-doc-fetch)

  # state
  (var w "last measured terminal width (columns)" 0)
  (var h "last measured height (rows)" 0)
  (var buf "line buffer" @"")
  (var prpt "prompt string (line prefix)" "")
  (def history "history stack. Top item is current placeholder." @[])
  (def tmp-buf "Buffer to group writes to stderr for terminal rendering." @"")
  (var pos "Cursor posiiton in buf" 0)
  (var lines-below "Number of dirty lines below input line for drawing cleanup." 0)
  (var ret-value "Value to return to caller, usually the mutated buffer." buf)
  (var more-input "Loop condition variable" true)

  (defn- flushs
    []
    (eprin tmp-buf)
    (eflush)
    (buffer/clear tmp-buf))

  (defn- clear
    []
    (eprin "\e[H\e[2J")
    (eflush))

  (defn- check-overflow
    []
    (def available-w (- w (length prpt)))
    (- (length buf) available-w -1))

  (defn- refresh
    []
    (def overflow (check-overflow))
    (def overflow-right (- (length buf) pos))
    (def overflow-right (if (< overflow overflow-right) overflow overflow-right))
    (def overflow-left (- overflow overflow-right))
    (def visual-pos
      (if (pos? overflow)
        (+ (length prpt) pos (- overflow-left))
        (+ (length prpt) pos)))
    (def visual-buf
      (if (pos? overflow)
        (string/slice buf overflow-left (if (< overflow-right 1) -1 (- overflow-right)))
        buf))
    (buffer/format tmp-buf "\r%s%s\e[0K\r\e[%dC" prpt visual-buf visual-pos)
    (flushs))

  (defn- clear-lines
    []
    (repeat lines-below
            (buffer/push tmp-buf "\e[1B\e[999D\e[K"))
    (when (pos? lines-below)
      (buffer/format tmp-buf "\e[%dA\e[999D" lines-below)
      (set lines-below 0))
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

  (defn- insert
    [c draw]
    (if (= (length buf) pos)
      (do
        (buffer/push buf c)
        (++ pos)
        (when draw
          (def o (check-overflow))
          (if (pos? o)
            (refresh)
            (do (buffer/push tmp-buf c) (flushs)))))
      (do
        (buffer/blit buf buf (inc pos) pos)
        (put buf pos c)
        (++ pos)
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
        (each b (slice choice (length ctx-string) -1)
          (insert b false))
        (refresh))
      (do # print all options
        (def gcp (reduce greatest-common-prefix (first options) options))
        (each b (string/slice gcp (length ctx-string) -1)
          (insert b false))
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
    []
    (when (> pos 0) (-- pos) (refresh)))

  (defn- kleftw
    []
    (while (and (> pos 0) (= 32 (buf (dec pos)))) (-- pos))
    (while (and (> pos 0) (not= 32 (buf (dec pos)))) (-- pos))
    (refresh))

  (defn- kright
    []
    (when (< pos (length buf)) (++ pos) (refresh)))

  (defn- krightw
    []
    (while (and (< pos (length buf)) (not= 32 (buf pos))) (++ pos))
    (while (and (< pos (length buf)) (= 32 (buf pos))) (++ pos))
    (refresh))

  (defn- kdelete
    [draw]
    (when (not= pos (length buf))
      (buffer/blit buf buf pos (inc pos))
      (buffer/popn buf 1)
      (if draw (refresh))))

  (defn- kdeletew
    []
    (while (and (< pos (length buf)) (= 32 (buf pos))) (kdelete false))
    (while (and (< pos (length buf)) (not= 32 (buf pos))) (kdelete false))
    (refresh))

  (defn- kback
    [draw]
    (when (pos? pos)
      (-- pos)
      (buffer/blit buf buf pos (inc pos))
      (buffer/popn buf 1)
      (if draw (refresh))))

  (defn- kbackw
    []
    (while (and (> pos 0) (= 32 (buf (dec pos)))) (kback false))
    (while (and (> pos 0) (not= 32 (buf (dec pos)))) (kback false))
    (refresh))

  (fn getline-fn
    [&opt prompt buff _]
    (set buf (or buff @""))
    (set prpt (string prompt))
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
        (def c (rawterm/getch))
        (def [_h _w] (rawterm/size))
        (set w _w)
        (set h _h)
        (if (>= c 0x20)
          (case c
            127 # backspace
            (kback true)
            # default - keep default case not at bottom of case (micro-opt)
            (when (>= c 0x20)
              (insert c true)))
          (case c
            1 # ctrl-a
            (do (set pos 0) (refresh))
            2 # ctrl-b
            (kleft)
            3 # ctrl-c
            (do (clear-lines) (eprint "^C") (eflush) (rawterm/end) (os/exit 1))
            4 # ctrl-d, eof
            (do (set more-input false) (clear-lines))
            5 # ctrl-e
            (do (set pos (length buf)) (refresh))
            6 # ctrl-f
            (kright)
            7 # ctrl-g
            (showdoc)
            8 # ctrl-h
            (kback true)
            9 # tab
            (autocomplete)
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
            26 # ctrl-z
            (do (rawterm/ctrl-z) (refresh))
            27 # escape sequence, process more
            (case (rawterm/getch)
              (chr "[")
              (let [c3 (rawterm/getch)]
                (cond
                  (and (>= c3 (chr "0")) (<= c3 (chr "9")))
                  (case (rawterm/getch)
                    (chr "1") (do (set pos 0) (refresh))
                    (chr "3") (kdelete true)
                    (chr "4") (do (set pos (length buf)) (refresh)))
                  (= c3 (chr "O"))
                  (case (rawterm/getch)
                    (chr "H") (do (set pos 0) (refresh))
                    (chr "F") (do (set pos (length buf)) (refresh)))
                  (= c3 (chr "A")) (set hindex (history-move hindex -1))
                  (= c3 (chr "B")) (set hindex (history-move hindex 1))
                  (= c3 (chr "C")) (kright)
                  (= c3 (chr "D")) (kleft)
                  (= c3 (chr "H")) (do (set pos 0) (refresh))
                  (= c3 (chr "F")) (do (set pos (length buf)) (refresh))))
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

