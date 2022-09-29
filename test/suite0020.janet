(use ../spork/test)
(import ../spork/path)

(start-suite 20)

(assert (= (path/posix/relpath "dir1" "dir2") "../dir2"))

(assert (= (path/win32/relpath "dir1" "dir2") "..\\dir2"))

(assert (= (path/relpath "dir1" "dir2") (path/join ".." "dir2")))

(assert (= (path/posix/relpath "a/bit/deeper/with/some/nested/dir" "a/bit/deeper/with/other/nested/dir") "../../../other/nested/dir"))

(assert-docs "/spork/test")
(assert-docs "/spork/argparse")
(assert-docs "/spork/fmt")
(assert-docs "/spork/generators")
(assert-docs "/spork/getline")
(assert-docs "/spork/htmlgen")
(assert-docs "/spork/http")
(assert-docs "/spork/httpf")
(assert-docs "/spork/misc")
(assert-docs "/spork/msg")
(assert-docs "/spork/netrepl")
(assert-docs "/spork/path")
(assert-docs "/spork/regex")
(assert-docs "/spork/rpc")
(assert-docs "/spork/schema")
(assert-docs "/spork/sh")
(assert-docs "/spork/tasker")
(assert-docs "/spork/temple")

(assert-docs "spork/json")
(assert-docs "spork/tarray")
(assert-docs "spork/rawterm")
(assert-docs "spork/utf8")

(end-suite)
