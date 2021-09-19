(use ../spork/test)
(import ../spork/msg)
(import ../spork/netrepl)

(start-suite 9)

(with [wt (netrepl/server "localhost" "8000")]
  (with [s (net/connect "localhost" "8000")]
    (def recv (msg/make-recv s))
    (def send (msg/make-send s))
    (send "test")

    (assert (= (recv) "test:1: ") "Prompt 1")
    (send "(+ 1 2)\n")
    (assert (= (recv) "\e[32m3\e[0m\n") "Result 1")

    (assert (= (recv) "test:2: ") "Prompt 2")
    (send "\xFF(parser/where (dyn :parser) 100)")
    (assert (= (recv) "(true (100 0))") "Response 2")
    (send "(+ 1 2)\n")
    (assert (= (recv) "\e[32m3\e[0m\n") "Result 2")

    (assert (= (recv) "test:101: ") "Prompt 3")
    (send "\xFEcancel")
    (assert (= (recv) nil) "Response 3")

    (assert (= (recv) "test:101: ") "Prompt 4")
    (send "\xFEsource \"foobar.janet\"")
    (assert (= (recv) nil) "Response 4")

    (assert (= (recv) "test:101: ") "Prompt 5")
    (send "(def foo :bar)\n")
    (assert (= (recv) "\e[33m:bar\e[0m\n") "Result 5")

    (assert (= (recv) "test:102: ") "Prompt 6")
    (send "(get (dyn 'foo) :source-map)\n")
    (assert (= (recv) "(\e[35m\"foobar.janet\"\e[0m \e[32m101\e[0m \e[32m1\e[0m)\n") "Result 6")

    (assert (= (recv) "test:103: ") "Prompt 7")))

(end-suite)
