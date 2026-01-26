###
### wchar_procunicode: Generate a character width table from Unicode data.
###
# Usage:
# 1. Obtain a version of the Unicode Character Database, e.g.,
#    from https://www.unicode.org/Public/UCD/latest/.
# 2. Invoke this script with paths to UnicodeData.txt and EastAsianWidth.txt.
# 3. Paste the resulting output into src/getline.c as k_width_classes[].
#
# With thanks to Justine Tunney (https://github.com/jart; https://justine.lol)
# for assistance and her great work on Cosmopolitan Libc/Bestline.

(defn strip-comment [line]
  (def line (string/trimr line))
  (if-let [comment-start (string/find "#" line)]
    (string/trimr (slice line 0 comment-start))
    line))

(defn parse-line-unicode-data [line]
  (def parts (string/split ";" (strip-comment line)))
  (when (not= 15 (length parts))
    (errorf "input does not seem like Unicode data: %s" line))
  (def codepoint (first parts))
  (def class (parts 2))
  (when (string/find ".." codepoint)
    (errorf "input is not Unicode data: codepoint %s is actually a range" codepoint))
  [(scan-number codepoint 16) class])

(defn parse-line-width-data [line]
  (def parts (string/split ";" (strip-comment line)))
  (when (not= 2 (length parts))
    (break nil))
  (def [cprange class] parts)
  (def cprange
    (if-let [range (string/find ".." cprange)]
      (let [[a b] [(slice cprange 0 range) (slice cprange (+ 2 range))]]
        [(scan-number a 16) (scan-number b 16)])
      [(scan-number cprange 16) (scan-number cprange 16)]))
  [cprange class])

(defn width-for-char [c [general-category east-asian-width]]
  (cond
    (= c 0) 0
    (= "Cc" general-category) -1
    (or (= "W" east-asian-width)
        (= "F" east-asian-width)) 2
    (= c 0x00AD) 1
    (or (= "Me" general-category)
        (= "Mn" general-category)
        (= "Cf" general-category)
        (<= 0x1160 c 0x11FF)) 0
    1))

(defn bitset/new [] @"\x80\0\0\0\0\0\0\0")
(defn bitset/pos [pos]
  (def pos (inc pos)) # top bit is masked
  (when (>= pos 64) (errorf "pos %d out of range" pos))
  [(brshift pos 3) (- 7 (band pos 7))])
(defn bitset/test [mask pos]
  (def [byte bit] (bitset/pos pos))
  (def byte-val (mask byte))
  (def bit-val (band byte-val (blshift 1 bit)))
  (not= bit-val 0))
(defn bitset/set [mask pos]
  (def [byte bit] (bitset/pos pos))
  (put mask byte
       (bor (mask byte) (blshift 1 bit))))
(defn bitset/clear [mask pos]
  (def [byte bit] (bitset/pos pos))
  (put mask byte
       (band (mask byte) (bxor 0xFF (blshift 1 bit)))))
(defn buffer->array [buf]
  (def a (array/new (length buf)))
  (each x buf
    (array/push a x))
  a)

(defn coalesce! [triples]
  (comment
    ```
    struct width_table_entry {
      uint32_t start_point; // always a codepoint, used as a sorting key
      uint32_t width;
      // if top bit is set, bits 63..0 indicate for which codepoints after
      // start_point this applies to
      // otherwise the value is the literal end point
      uint64_t end_point_or_bitmask;
    };
    ```)
  (var -start nil)
  (var -end nil)
  (var -mask nil)
  (var -width nil)
  (var -coalesced nil)
  (def entries @[])
  (var i 0)

  (defn begin [start end width]
    (set -start start)
    (set -end end)
    (set -width width)
    (set -coalesced 1)
    (when (< (- end start) 63)
      (set -mask (bitset/new))
      (for i start (inc end)
        (bitset/set -mask (- i start)))))
  (defn flush [start end width]
    (if (> -coalesced 1)
      (array/push entries {:start -start :mask -mask :width -width})
      (array/push entries {:start -start :end -end :width -width}))
    (begin start end width))
  (defn try-coalesce [start end]
    (if (< (- end -start) 63)
      (do
        (for i start (inc end)
          (bitset/set -mask (- i -start)))
        (++ -coalesced)
        true)
      false))

  (each [start end width] triples
    (if -width
      (if (not= width -width)
        (flush start end width)
        (if (try-coalesce start end)
          (do) # noop
          (flush start end width)))
      (begin start end width)))

  (each {:start start :end end :mask mask :width width} entries
    (if mask
      (printf "{ %6d, %2d, 0x%02x%02x%02x%02x%02x%02x%02x%02xULL },"
              start width ;(buffer->array mask))
      (printf "{ %6d, %2d, %18d    }," start width end))))

(defn main [_ path-unicode-data path-width-data]
  (def chars (array/new 0x10FFFF))
  (with [f (file/open path-unicode-data :r)]
    (forever
      (def line (:read f :line))
      (when (nil? line) (break))
      (def line (parse-line-unicode-data line))
      (when line
        (def [ch cl] line)
        (while (< (length chars) ch)
          (array/push chars [nil nil]))
        (array/push chars [cl nil]))))
  (with [f (file/open path-width-data :r)]
    (forever
      (def line (:read f :line))
      (when (nil? line) (break))
      (def line (parse-line-width-data line))
      (when line
        (def [[a b] cl-b] line)
        (for i a (inc b)
          (def [cl-a _] (in chars i [nil nil]))
          (put chars i [cl-a cl-b])))))

  (def triples @[])
  (var run-width 1)
  (var run-start nil)
  (for c 0 (length chars)
    (def width (width-for-char c (in chars c)))
    (when (not= width run-width)
      (when (not= run-width 1)
        (array/push triples [run-start (dec c) run-width]))
      (set run-width width)
      (set run-start c)))
  (when (not= run-width 1)
    (array/push triples [run-start (dec (length chars)) run-width]))

  (print "/* AUTO-GENERATED BY tools/wchar_procunicode.janet */")
  (coalesce! triples)

  nil)
