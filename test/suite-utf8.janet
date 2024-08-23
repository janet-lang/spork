(import spork/utf8)
(use ../spork/test)

(start-suite)

###
### utf8/decode-rune
###

(eachp [enc dec]
       {"a" (chr "a")
        "á" 0xE1
        "ა" 0x10D0
        "𐊀" 0x10280}
  (assert (= [dec (length enc)] (utf8/decode-rune enc))
          (string/format "utf8: decode (U+%X)" dec)))

(each inv
      ["\x81" # stray continuation
       "\xf0\x90\x8a" "\xf0\x90\x8a" "\xf0\x90" "\xf0" # truncated forward
       "\x90\x8a\x80" "\x8a\x80" "\x80" # truncated backward
       "\xfe" "\xff"] # invalid
  (assert (= [nil 0] (utf8/decode-rune inv))
          (string/format "utf8: decode invalid (%q)" inv)))

(defn- decode-iterate [buf]
  (var i 0)
  (def iter @[])
  (while (< i (length buf))
    (def [ch i+] (utf8/decode-rune buf i))
    (when (nil? ch)
      (errorf "invalid UTF-8 sequence: %q (pos %d)" buf i))
    (+= i i+)
    (array/push iter ch))
  iter)
(eachp [utf8 codepoints]
       {"ķēķī" @[0x137 0x113 0x137 0x12B]
        "チェリー" @[0x30C1 0x30A7 0x30EA 0x30FC]
        "🤣😜🐕" @[0x1F923 0x1F61C 0x1F415]
        "あaá🇦" @[0x3042 0x61 0xE1 0x1F1E6]}
  (assert (deep= codepoints (decode-iterate utf8))
          (string/format "utf8: decode iterate (%q)" codepoints)))

(let [enc "あ"]
  (loop [i :range [1 3]]
    (assert (= [nil 0] (utf8/decode-rune enc i))
            (string/format "utf8: decode from truncated start (%d)" i))))

###
### utf8/prefix->width
###

(eachp [prefix len]
       {0x7F 1
        0x80 1 # invalid (truncated)
        0xDF 2
        0xEF 3
        0xF7 4
        0xFB 1} # invalid (too long)
  (assert (= len (utf8/prefix->width prefix))
          (string/format "utf8: prefix->width (%X)" prefix)))

###
### utf8/encode-rune
###
(assert (deep= @"a" (utf8/encode-rune (chr "a")))
        "utf8: encode simple")
(assert (deep= @"\xC3\xA1" (utf8/encode-rune 0xE1))
        "utf8: encode 2byte")
(assert (deep= @"\xE3\x81\x82" (utf8/encode-rune 0x3042))
        "utf8: encode 3byte")
(assert (deep= @"\xF0\x92\x81\x9E" (utf8/encode-rune 0x1205E))
        "utf8: encode 4byte")
(let [[ok err] (protect (utf8/encode-rune 0x110000))]
  (assert (false? ok)
          (string/format "utf8: encode overlong (got %q)" err)))

# buffer reuse
(let [b @"a"]
  (assert (= b (utf8/encode-rune 0xE1 b))
          "utf8: encode reuse buffer")
  (assert (deep= @"aá" b)
          "utf8: encode reuse buffer (encoding result)"))

(end-suite)
