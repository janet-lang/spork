(use ../spork/test)
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

(assert (regex/match `\a+` `Xy`) "match 7")
(assert (regex/match `\w+` `Xy0`) "match 8")
(assert (regex/match `cat|dog` "cat") "match 6")
(assert (regex/match `cat|dog` "dog") "match 7")
(assert (not (regex/match `cat|dog` "mouse")) "match 8")
(assert (regex/match `cat|dog|mouse` "mouse") "match 9")
(assert (regex/match `cat|dog|mouse` "cat") "match 10")
(assert (regex/match `cat|dog|mouse` "dog") "match 11")
(assert (regex/match `(cat|dog|mouse)+` "mousecatdog") "match 12")
(assert (regex/match `a(cat|dog|mouse)+` "amousecatdog") "match 13")

(end-suite)
