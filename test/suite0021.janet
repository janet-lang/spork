(use ../spork/test)
(import ../spork/base64)

(start-suite 21)

#base64/encode
(assert (= (base64/encode "this is a test") "dGhpcyBpcyBhIHRlc3Q="))
(assert (= (base64/encode "") ""))
(assert (= (base64/encode "f") "Zg=="))
(assert (= (base64/encode "fo") "Zm8="))
(assert (= (base64/encode "foo") "Zm9v"))
(assert (= (base64/encode "foob") "Zm9vYg=="))
(assert (= (base64/encode "fooba") "Zm9vYmE="))
(assert (= (base64/encode "foobar") "Zm9vYmFy"))

#base64/decode
(assert (= (base64/decode "dGhpcyBpcyBhIHRlc3Q=") "this is a test"))
(assert (= (base64/decode "") ""))
(do (def some-string "\x1Cdawdawdadwdaw\xB0")
    (assert (= (base64/decode (base64/encode some-string)) some-string)))

(end-suite)
