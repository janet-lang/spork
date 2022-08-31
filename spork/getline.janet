###
### A Janet implementation of `(getline)` that is non-blocking.
### Allows for beter integration in netrepl (completions and docs can be streamed
### over the network).
###

### TODO
# - unit testing?
# - character width detection for better utf-8 support

(import spork/rawterm)
(import spork/utf8)

###
### Unicode tables
###
# Incorporates data from Bestline, used in accordance to this license.
#
# Copyright 2018-2021 Justine Tunney <jtunney@gmail.com>
# Copyright 2010-2016 Salvatore Sanfilippo <antirez@gmail.com>
# Copyright 2010-2013 Pieter Noordhuis <pcnoordhuis@gmail.com>
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

(def- bestline-license
  (comptime (string
    "Bestline (BSD-2)\n"
    "Copyright 2018-2020 Justine Tunney <jtunney@gmail.com>\n"
    "Copyright 2010-2016 Salvatore Sanfilippo <antirez@gmail.com>\n"
    "Copyright 2010-2013 Pieter Noordhuis <pcnoordhuis@gmail.com>\n")))
(defn- bestline-noop [&])

(defn- char-control? [c]
  (or (<= c 0x1F) (<= 0x7F c 0x9F)))
(defn- char-width [c]
  (cond
    (char-control? c) 0
    (<= 0x1100 c 0x115f) 2
    (= 0x2329 c) 2
    (= 0x232a c) 2
    (and (<= 0x2e80 c 0xa4cf) (not= 0x303f c)) 2
    (<= 0xac00 c 0xd7a3) 2
    (<= 0xf900 c 0xfaff) 2
    (<= 0xfe10 c 0xfe19) 2
    (<= 0xfe30 c 0xfe6f) 2
    (<= 0xff00 c 0xff60) 2
    (<= 0xffe0 c 0xffe6) 2
    (<= 0x20000 c 0x2fffd) 2
    (<= 0x30000 c 0x3fffd) 2
    1))

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

(defn- monowidth
  "Get the monospace character width of a buffer. Assumes contents are UTF-8
  encoded."
  [buf]
  (var width 0)
  (var i 0)
  (def l (length buf))
  (while (< i l)
    (def [rune len] (utf8/decode-rune buf i))
    (+= i len)
    (+= width (char-width rune)))
  width)

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
  (var buf-width
    "Line buffer width in screen columns. Double-width characters cause
    this to increase by 2."
    0)
  (var prpt "prompt string (line prefix)" "")
  (var prpt-width "prompt string character width" 0)
  (def history "history stack. Top item is current placeholder." @[])
  (def tmp-buf "Buffer to group writes to stderr for terminal rendering." @"")
  (var pos "Cursor byte position in buf" 0)
  (var wcursor
    "Cursor column position in buf. Double-width characters cause this to
    increase by 2."
    0)
  (var lines-below "Number of dirty lines below input line for drawing cleanup." 0)
  (var ret-value "Value to return to caller, usually the mutated buffer." buf)
  (var more-input "Loop condition variable" true)
  (def input-buf @"")

  (defn getc
    []
    (buffer/clear input-buf)
    (rawterm/getch input-buf)
    (def c (get input-buf 0))
    (var len (utf8/prefix->width c))
    (-- len)
    (while (not= len 0)
      (rawterm/getch input-buf)
      (-- len))
    (def [rune _] (utf8/decode-rune input-buf))
    rune)

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
    (def available-w (- w prpt-width))
    (- buf-width available-w))

  (defn- refresh
    []
    (def overflow (check-overflow))
    (def width-under-cursor
      (if (= pos (length buf))
        1 # implicit space
        (let [[rune _] (utf8/decode-rune buf pos)]
          (char-width rune))))
    (def overflow (+ overflow width-under-cursor))
    (def overflow-right (- buf-width wcursor))
    (def overflow-right (if (< overflow overflow-right) overflow overflow-right))
    (def overflow-left (- overflow overflow-right))
    # If the cursor is on the right side but not at EOL, additionally show the
    # character under the cursor.
    (def overflow-right (- overflow-right width-under-cursor))
    (def visual-pos
      (if (pos? overflow)
        (+ prpt-width wcursor (- overflow-left))
        (+ prpt-width wcursor)))
    (var overtrimmed-left? false)
    (def visual-buf
      (if (pos? overflow)
        (do
          (def start # transform cursor position to byte position
            (do
              (var i 0)
              (var w 0)
              (while (< w overflow-left)
                (def [rune len] (utf8/decode-rune buf i))
                (+= i len)
                (+= w (char-width rune)))
              (when (not= w overflow-left)
                (set overtrimmed-left? true))
              i))
          # If we got one extra column freed up because the left side was
          # overtrimmed, allow it to be untrimmed here.
          (def overflow-right
            (if overtrimmed-left? (dec overflow-right) overflow-right))
          (def end
            (if (< overflow-right 1) -1
              (do
                (var i (length buf))
                (var w 0)
                (while (< w overflow-right)
                  (def [rune len] (utf8/decode-rune-reverse buf i))
                  (-= i len)
                  (+= w (char-width rune)))
                i)))
          (string/slice buf start end))
        buf))
    (buffer/format tmp-buf "\r%s%s\e[0K\r\e[%dC" prpt visual-buf
      (if overtrimmed-left? (dec visual-pos) visual-pos))
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
    "If from-input-buf? is truthy, as a microopt, copies the input character
    from input-buf, assuming that it contains only one."
    [c draw &opt from-input-buf?]
    (default from-input-buf? false)
    (if (= (length buf) pos)
      (do
        (if from-input-buf?
          (buffer/push buf input-buf)
          (utf8/encode-rune c buf))
        (set pos (length buf))
        (def w (char-width c))
        (+= wcursor w)
        (+= buf-width w)
        (when draw
          (def o (check-overflow))
          (if (pos? o)
            (refresh)
            (do
              (if from-input-buf?
                (buffer/push tmp-buf input-buf)
                (utf8/encode-rune c tmp-buf))
              (flushs)))))
      (do
        (def ch
          (if from-input-buf?
            input-buf
            (utf8/encode-rune c)))
        (def l (length buf))
        (def need-expand (- (length ch) (- (length buf) pos)))
        (when (pos? need-expand)
          (buffer/push buf (string/repeat "\0" need-expand)))
        (buffer/blit buf buf (+ pos (length ch)) pos l)
        (buffer/blit buf ch pos)
        (def w (char-width c))
        (+= pos (length ch))
        (+= wcursor w)
        (+= buf-width w)
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
        (utf8/each-rune c (slice choice (length ctx-string) -1)
          (insert c false))
        (refresh))
      (do # print all options
        (def gcp (reduce greatest-common-prefix (first options) options))
        (utf8/each-rune c (string/slice gcp (length ctx-string) -1)
          (insert c false))
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
    [draw]
    (when (> wcursor 0)
      (def [c len] (utf8/decode-rune-reverse buf pos))
      (-= wcursor (char-width c))
      (-= pos len)
      (if draw (refresh))))

  (defn- kleftw
    []
    (while (and (> wcursor 0) (= 32 (buf (dec pos)))) (kleft false))
    (while (and (> wcursor 0) (not= 32 (buf (dec pos)))) (kleft false))
    (refresh))

  (defn- kright
    [draw]
    (when (< pos (length buf))
      (def [c len] (utf8/decode-rune buf pos))
      (+= wcursor (char-width c))
      (+= pos len)
      (if draw (refresh))))

  (defn- krightw
    []
    (while (and (< wcursor (length buf)) (not= 32 (buf pos))) (kright false))
    (while (and (< wcursor (length buf)) (= 32 (buf pos))) (kright false))
    (refresh))

  (defn- khome
    []
    (set pos 0)
    (set wcursor 0)
    (refresh))

  (defn- kend
    []
    (set pos (length buf))
    (set wcursor buf-width)
    (refresh))

  (defn- kdelete
    [draw]
    (when (not= pos (length buf))
      (def [c len] (utf8/decode-rune buf pos))
      (buffer/blit buf buf pos (+ pos len))
      (buffer/popn buf len)
      (-= buf-width (char-width c))
      (if draw (refresh))))

  (defn- kdeletew
    []
    (while (and (< pos (length buf)) (= 32 (buf pos))) (kdelete false))
    (while (and (< pos (length buf)) (not= 32 (buf pos))) (kdelete false))
    (refresh))

  (defn- kback
    [draw]
    (when (pos? pos)
      (def [c len] (utf8/decode-rune-reverse buf pos))
      (def w (char-width c))
      (-= wcursor w)
      (-= pos len)
      (-= buf-width w)
      (buffer/blit buf buf pos (+ pos len))
      (buffer/popn buf len)
      (if draw (refresh))))

  (defn- kbackw
    []
    (while (and (> pos 0) (= 32 (buf (dec pos)))) (kback false))
    (while (and (> pos 0) (not= 32 (buf (dec pos)))) (kback false))
    (refresh))

  (fn getline-fn
    [&opt prompt buff _]

    # Ensure the Bestline license notice is included in the bytecode.
    (bestline-noop bestline-license)

    (set buf (or buff @""))
    (set prpt (string prompt))
    (set prpt-width (monowidth prpt))
    (unless (rawterm/isatty)
      (break (getline prpt buf)))
    (defer (rawterm/end)
      (rawterm/begin)
      (buffer/clear tmp-buf)
      (buffer/clear buf)
      (set buf-width 0)
      (set ret-value buf)
      (set pos 0)
      (set wcursor 0)
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
            127 # backspace
            (kback true)
            # default - keep default case not at bottom of case (micro-opt)
            (when (>= c 0x20)
              (insert c true true)))
          (case c
            1 # ctrl-a
            (khome)
            2 # ctrl-b
            (kleft)
            3 # ctrl-c
            (do (clear-lines) (eprint "^C") (eflush) (rawterm/end) (os/exit 1))
            4 # ctrl-d, eof
            (if (= pos (length buf))
              (do (set more-input false) (clear-lines))
              (kdelete true))
            5 # ctrl-e
            (kend)
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
                  (case (getc)
                    (chr "1") (khome)
                    (chr "3") (kdelete true)
                    (chr "4") (kend))
                  (= c3 (chr "O"))
                  (case (getc)
                    (chr "H") (khome)
                    (chr "F") (kend))
                  (= c3 (chr "A")) (set hindex (history-move hindex -1))
                  (= c3 (chr "B")) (set hindex (history-move hindex 1))
                  (= c3 (chr "C")) (kright true)
                  (= c3 (chr "D")) (kleft true)
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

