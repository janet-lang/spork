(use ../spork/test)
(import ../spork/pgp)

(start-suite)

(assert (string? (pgp/hex->word "01" 0)) "pgp/hex->word returns string")
(assert (= (pgp/hex->word "01" 0) "absurd") "pgp/hex->word returns pgp/word")
(assert (= (pgp/hex->word "0Y" 0) nil) "pgp/hex->word returns nil for wrong hex")
(assert (string? (pgp/word->hex "absurd")) "pgp/hex->word returns string")
(assert (deep= (pgp/hexs->words "01d1 02EE")
               @["absurd" "scavenger" "accrue" "universe"])
        "pgp/hex->words returns array of words")
(assert-error "pgp/hex->words errors out on wrong hex"
              (pgp/hexs->words "01d1 02YE"))
(assert (nil? (pgp/word->hex "absurdz"))
        "pgp/hex->word returns nil for unknown word")
(assert (= (pgp/word->hex "absurd") "01") "pgp/hex->word returns string")
(assert (deep= (pgp/words->hexs "absurd-scavenger accrue_universe upshot.village")
               @["01" "D1" "02" "EE" "F4" "F6"])
        "pgp/hex->words returns array of words")
(assert-error "pgp/hex->words errors out when there is unknown pgp/word"
              (pgp/words->hexs "absurdz-scavenger accrue_universe"))

(end-suite)
