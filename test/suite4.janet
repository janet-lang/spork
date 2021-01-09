(use ./helper)
(import ../spork/regex)

(start-suite 4)

(assert (regex/match `abc` `abcdefg`) "match 1")
(assert (regex/match `a.c` `azcdefg`) "match 2")
(assert (regex/match `a\s+c` `a    cdefg`) "match 3")
(assert (not (regex/match `a\s+c` `acdefg`)) "match 4")

(assert (regex/match `(?:abc){4}` "abcabcabcabc") "match 5")
(assert (deep= @["abc" "abc" "abc" "abc"]
               (regex/match `(?:(abc)){4}` "abcabcabcabc"))
        "match 6")

(end-suite)
