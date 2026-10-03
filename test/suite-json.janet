(use spork/test)
(import spork/json :as json)

(start-suite)

(defn check-object [x &opt z]
  (default z x)
  (def y (json/decode (json/encode x)))
  (def y1 (json/decode (json/encode x " " "\n")))
  (assert (deep= z y) (string/format "failed roundtrip 1: %p" x))
  (assert (deep= z y1) (string/format "failed roundtrip 2: %p" x)))

(check-object 1)
(check-object 100)
(check-object true)
(check-object false)
(check-object (range 1000))
(check-object @{"two" 2 "four" 4 "six" 6})
(check-object @{"hello" "world"})
(check-object @{"john" 1 "billy" "joe" "a" @[1 2 3 4 -1000]})
(check-object @{"john" 1 "∀abcd" "joe" "a" @[1 2 3 4 -1000]})
(check-object
  "ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ
ᛋᚳᛖᚪᛚ᛫ᚦᛖᚪᚻ᛫ᛗᚪᚾᚾᚪ᛫ᚷᛖᚻᚹᛦᛚᚳ᛫ᛗᛁᚳᛚᚢᚾ᛫ᚻᛦᛏ᛫ᛞᚫᛚᚪᚾ
ᚷᛁᚠ᛫ᚻᛖ᛫ᚹᛁᛚᛖ᛫ᚠᚩᚱ᛫ᛞᚱᛁᚻᛏᚾᛖ᛫ᛞᚩᛗᛖᛋ᛫ᚻᛚᛇᛏᚪᚾ᛬")
(check-object @["šč"])
(check-object "👎")

# Decoding utf-8 strings 
(assert (deep= "šč" (json/decode `"šč"`)) "did not decode utf-8 string correctly")

# Recursion guard
(def one @{:links @[]})
(def two @{:links @[one]})
(array/push (one :links) two)
(def objects @{:one one :two two})
(assert-error "error on cycles" (json/encode objects))

# null values
(check-object @{"result" :null})
(check-object {"result" :null} @{"result" :null})
(check-object :null)
(check-object nil :null)

# Byte-exact encoding: these properties are invisible to round-trip tests
(assert (= "0.1" (string (json/encode 0.1))) "float uses shortest round-trip form")
(assert (= "1" (string (json/encode 1))) "integer has no decimal point")
(assert (= "1000000" (string (json/encode 1e6))) "large integer without exponent")
(assert (= "1e+16" (string (json/encode 1e16))) "large integral float uses exponent form")
(assert (= "2.5e-10" (string (json/encode 2.5e-10))) "small float uses shortest form")
(assert (= "0.30000000000000004" (string (json/encode 0.30000000000000004))) "floats needing 17 digits stay exact")

# Standard short escapes for common control characters
(assert (= "\"a\\nb\"" (string (json/encode "a\nb"))) "newline uses short escape")
(assert (= "\"\\t\\n\\r\\b\\f\"" (string (json/encode "\t\n\r\b\f"))) "named control characters use short escapes")
(assert (= "\"\\u001B\"" (string (json/encode "\x1b"))) "other control characters use \\u escape")

# Object keys are sorted for deterministic, readable output
(assert (= "{\"a\":1,\"b\":2}" (string (json/encode @{"b" 2 "a" 1}))) "table keys sorted")
(assert (= "{\"a\":1,\"b\":2}" (string (json/encode {"b" 2 "a" 1}))) "struct keys sorted")
(assert (= "{\r\n  \"a\": 1,\r\n  \"b\": 2\r\n}" (string (json/encode @{"b" 2 "a" 1} "  "))) "sorted keys with indent")

# Non-finite numbers have no JSON representation
(assert-error "error on nan" (json/encode math/nan))
(assert-error "error on infinity" (json/encode math/inf))

(end-suite)
