(use ../spork/test)
(import ../spork/http :as http)

(start-suite 12)

(defn- test-http-item
  [x
   expected-method
   expected-path
   expected-status
   expected-headers]
  (assert (not= :error x) "invalid http")
  (when (not= :error x)
    (assert (= expected-path (in x :path))
            (string "bad path, expected " expected-path ", got " (in x :path)))
    (assert (= expected-headers (freeze (in x :headers)))
            (string/format "bad headers: %.99p" (freeze (in x :headers))))
    (assert (= expected-method (in x :method))
            (string "bad status, expected " expected-status ", got " (in x :status)))
    (assert (= expected-method (in x :method))
            (string "bad method, expected " expected-method ", got " (in x :method)))))

(defn- test-http-parse
  [parser
   payload
   expected-method
   expected-path
   expected-status
   expected-headers]
  (def x (parser nil payload))
  (test-http-item x expected-method expected-path expected-status expected-headers))

(test-http-parse http/read-request @"GET / HTTP/1.0\r\n\r\n" "GET" "/" nil {})
(test-http-parse http/read-request @"GET /abc.janet HTTP/1.0\r\n\r\n" "GET" "/abc.janet" nil {})
(test-http-parse http/read-request @"GET /abc.janet HTTP/1.0\r\na:b\r\n\r\n" "GET" "/abc.janet" nil {"a" "b"})
(test-http-parse http/read-request @"POST /abc.janet HTTP/1.0\r\na:b\r\n\r\nextraextra" "POST" "/abc.janet" nil {"a" "b"})
(test-http-parse http/read-response @"HTTP/1.0 200 OK\r\na:b\r\n\r\nextraextra" nil nil 200 {"a" "b"})

(defn simple-server
  [req]
  (case (in req :method)
    "GET" {:status 200 :body (in req :path)}
    "POST" {:status 200 :body (http/read-body req)}))

(with [server (http/server simple-server "127.0.0.1" 9816)]
  (test-http-item (http/request "GET" "http://127.0.0.1:9816") nil nil 200
                  {"content-length" "1"}))

(defn body-server
  [req]
  {:status 200 :body (string (http/read-body req))})

(with [server (http/server body-server "127.0.0.1" 9816)]
  (def res (http/request "POST" "http://127.0.0.1:9816" :body "Strong and healthy"))
  (test-http-item res nil nil 200 {"content-length" "18"}))

(with [server (http/server body-server "127.0.0.1" 9816)]
  (def res (http/request "POST" "http://127.0.0.1:9816" :body (string/repeat "a" 2097152)))
  (test-http-item res nil nil 200 {"content-length" "2097152"}))

(with [server (http/server body-server "127.0.0.1" 9816)]
  (def res (http/request "POST" "http://127.0.0.1:9816" :body (string/repeat "a" 4194304)))
  (test-http-item res nil nil 200 {"content-length" "4194304"}))

(defn- chunk
  [data]
  (string/format "%x\r\n%s\r\n" (length data) data))
(defn- close-both
  [[r w]]
  (:close r)
  (:close w))
(with [[r w] (os/pipe) close-both]
  (http/send-response w {:status 200
                         :body ["a" (string/repeat "a" 16) (string/repeat "a" 256)]})
  (:close w)
  (assert
    (deep= (:read r :all)
      (buffer
        "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
        (chunk "a")
        (chunk (string/repeat "a" 16))
        (chunk (string/repeat "a" 256))
        (chunk "")))
    "write-body: chunked encoding"))

(with [[r w] (os/pipe) close-both]
  (each len [1 16 256]
    (:write w (chunk (string/repeat "a" len))))
  (:write w "0\r\n\r\n")
  (:close w)
  (assert
    (deep= (buffer (string/repeat "a" (+ 1 16 256)))
      (http/read-body @{:buffer (buffer/new 0)
                        :connection r
                        :headers {"transfer-encoding" "chunked"}}))
    "read-body: chunked encoding, basic"))

# Chunked encoding across packet boundaries: unusual but not impossible.
(with [[r w] (os/pipe) close-both]
  (def init @"4\r\nabcd\r\n")
  (:write w @"4\r\nabcd\r\n0\r\n\r\n")
  (:close w)
  (assert
    (deep= @"abcdabcd" (http/read-body @{:buffer init
                                         :connection r
                                         :headers {"transfer-encoding" "chunked"}}))
    "read-body: chunked encoding, full chunk in first packet"))
(with [[r w] (os/pipe) close-both]
  (def init @"4\r\nab")
  (:write w @"cd\r\n0\r\n\r\n")
  (:close w)
  (assert
    (deep= @"abcd" (http/read-body @{:buffer init
                                     :connection r
                                     :headers {"transfer-encoding" "chunked"}}))
    "read-body: chunked encoding, partial chunk in first packet"))
(with [[r w] (os/pipe) close-both]
  (def init @"4\r\nabcd\r\n4\r\nabcd\r\n4\r\nab")
  (:write w @"cd\r\n0\r\n\r\n")
  (:close w)
  (assert
    (deep= @"abcdabcdabcd" (http/read-body @{:buffer init
                                             :connection r
                                             :headers {"transfer-encoding" "chunked"}}))
    "read-body: chunked encoding, full chunks + partial chunk in first packet"))

# Test the query string grammar by itself.
(assert (deep= @[@{"a" " "}] (peg/match http/query-string-grammar "a=%20")) "query string grammar 1")
(assert (deep= @[@{"a" " " "b" true}] (peg/match http/query-string-grammar "a=%20&b")) "query string grammar 2")
(assert (deep= @[@{"a" " " "b" true}] (peg/match http/query-string-grammar "a=%20;b")) "query string grammar 3")
(assert (deep= @[@{"a" " " "b" "once upon a time"}] (peg/match http/query-string-grammar "a=%20&b=once+upon+a+time")) "query string grammar 4")
(assert (deep= @[@{"a" " " "bedtime story" "once upon a time"}] (peg/match http/query-string-grammar "a=%20&bedtime+story=once+upon+a+time")) "query string grammar 4")

(end-suite)
