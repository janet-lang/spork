###
### string.janet
###
### String helper functions
###

(defn trim-prefix
  "trim the specified prefix of a string if it has one"
  [prefix str]
  (if (string/has-prefix? prefix str)
      (slice str (length prefix) -1)
      str))

(defn trim-suffix
  "trim the specified suffix of a string if it has one"
  [suffix str]
  (if (string/has-suffix? suffix str)
      (slice str 0 (* -1 (+ 1 (length suffix))))
      str))
