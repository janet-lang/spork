(use ../spork/test)
(import spork/base64)

(start-suite)

(assert-docs "spork/base64")

(eachp
  [decoded encoded]
  {"this is a test" "dGhpcyBpcyBhIHRlc3Q="
   "" ""
   "f" "Zg=="
   "fo" "Zm8="
   "foo" "Zm9v"
   "foob" "Zm9vYg=="
   "fooba" "Zm9vYmE="
   "foobar" "Zm9vYmFy"
   "\x1Cdawdawdadwdaw\xB0" "HGRhd2Rhd2RhZHdkYXew"}
  (assert (= (base64/decode encoded) decoded))
  (assert (= (base64/encode decoded) encoded)))
(assert (= "Wrong length: 1" (last (protect (base64/decode "A")))))
(assert (= "Wrong character: %" (last (protect (base64/decode "A%==")))))

(end-suite)
