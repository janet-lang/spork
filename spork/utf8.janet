###
### Minimal UTF-8 decoding/encoding routines in pure Janet.
###

# Terminology:
# - Rune: A Unicode codepoint. A single integer with value <= 0x10FFFF.

(defn decode-rune
  "Read a UTF-8 encoded Unicode codepoint from the buffer which starts at the
  given index. Returns a tuple [value width], where width = number of bytes
  consumed. If at the end of buffer, returns [nil 0]. Assumes that the input
  is valid UTF-8 and does not perform validation."
  [buf &opt start]
  (default start 0)
  (when (>= start (length buf))
    (break [nil 0]))
  (var i (dec start))
  (def b (buf (++ i)))
  (tuple
    (cond
      (= (band b 2r1000_0000) 0) b
      (= (band b 2r111_00000) 2r110_00000)
        (bor
          (blshift (band b 2r000_11111) 6)
          (band (buf (++ i)) 2r00_111111))
      (= (band b 2r1111_0000) 2r1110_0000)
        (bor
          (blshift (band b 2r0000_1111) 12)
          (blshift (band (buf (++ i)) 2r00_111111) 6)
          (band (buf (++ i)) 2r00_111111))
      #(= (band b 2r11111_000) 2r11110_000)
      (bor
        (blshift (band b 2r00000_111) 18)
        (blshift (band (buf (++ i)) 2r00_111111) 12)
        (blshift (band (buf (++ i)) 2r00_111111) 6)
        (band (buf (++ i)) 2r00_111111)))
    (- i start -1)))

(defn encode-rune
  "Encode a Unicode codepoint into the end of a buffer."
  [rune &opt buf]
  (default buf (buffer/new 4))
  (cond
    (<= 0 rune 0x7f) (buffer/push buf rune)
    (<= 0x80 rune 0x7ff)
      (buffer/push buf
        (bor 2r110_00000 (band (brshift rune 6) 2r000_11111))
        (bor 2r10_000000 (band rune 2r00_111111)))
    (<= 0x800 rune 0xffff)
      (buffer/push buf
        (bor 2r1110_0000 (band (brshift rune 12) 2r0000_1111))
        (bor 2r10_000000 (band (brshift rune 6) 2r00_111111))
        (bor 2r10_000000 (band rune 2r00_111111)))
    (<= 0x10000 rune 0x10ffff)
      (buffer/push buf
        (bor 2r11110_000 (band (brshift rune 18) 2r00000_111))
        (bor 2r10_000000 (band (brshift rune 12) 2r00_111111))
        (bor 2r10_000000 (band (brshift rune 6) 2r00_111111))
        (bor 2r10_000000 (band rune 2r00_111111)))
    (errorf "character %x outside UTF-8 range" rune)))

(defn prefix->width
  "Given the first byte in an UTF-8 sequence, get the number of bytes that the
  codepoint sequence takes up, including the prefix byte."
  [c]
  (cond
    (= (band c 2r11111_000) 2r11110_000) 4
    (= (band c 2r1111_0000) 2r1110_0000) 3
    (= (band c 2r111_00000) 2r110_00000) 2
    (= (band c 2r1_0000000) 2r0_0000000) 1
    (errorf "invalid UTF-8 initial byte: %x" c)))

(defn decode-rune-reverse
  "Similar to decode-rune, but operates in the buffer backwards. The start index
  must be 1 larger than the index of the final byte; it defaults to the end of
  the buffer."
  [buf &opt start]
  (default start (length buf))
  (when (<= start 0)
    (break [nil 0]))
  (var i (dec start))
  (var len 1)
  (var rune 0)
  (var offset 0)
  # Pop all trailing bytes (with 2r10_000000 as their high bits) and accumulate
  # their values.
  (while (<= 0 i)
    (def ch (buf i))
    (if (= (band ch 2r11_000000) 2r10_000000)
      (do
        (set rune (bor rune (blshift (band ch 2r00_111111) offset)))
        (+= offset 6)
        (++ len)
        (-- i))
      (break)))
  # If no initial byte exists, the sequence is incomplete.
  (when (or (empty? buf) (< i 0))
    (break [nil 0]))

  (def ch (buf i))
  (set rune
    (cond
      (= (band ch 2r11111_000) 2r11110_000)
        (bor rune (blshift (band ch 2r00000_111) offset))
      (= (band ch 2r1111_0000) 2r1110_0000)
        (bor rune (blshift (band ch 2r0000_1111) offset))
      (= (band ch 2r111_00000) 2r110_00000)
        (bor rune (blshift (band ch 2r000_11111) offset))
      ch))
  [rune len])

(defn pop-rune
  "Pop a Unicode codepoint from a buffer. Returns the rune value."
  (def [rune len] (decode-rune-rev buf))
  (when rune
    (buffer/popn buf len))
  rune)

(defmacro each-rune
  "Runs body with x bound to every rune in str."
  [x str & body]
  (with-syms [$i $l $w]
    ~(do
      (var ,$i 0)
      (def ,$l (length ,str))
      (while (< ,$i ,$l)
        (def [,x ,$w] (,decode-rune ,str ,$i))
        (do ,;body)
        (+= ,$i ,$w))
      nil)))
