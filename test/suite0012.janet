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

(test-http-parse http/read-request "GET / HTTP/1.0\r\n\r\n" "GET" "/" nil {})
(test-http-parse http/read-request "GET /abc.janet HTTP/1.0\r\n\r\n" "GET" "/abc.janet" nil {})
(test-http-parse http/read-request "GET /abc.janet HTTP/1.0\r\na:b\r\n\r\n" "GET" "/abc.janet" nil {"a" "b"})
(test-http-parse http/read-request "POST /abc.janet HTTP/1.0\r\na:b\r\n\r\nextraextra" "POST" "/abc.janet" nil {"a" "b"})
(test-http-parse http/read-response "HTTP/1.0 200 OK\r\na:b\r\n\r\nextraextra" nil nil 200 {"a" "b"})

(defn simple-server
  [req]
  (case (in req :method)
    "GET" {:status 200 :body (in req :path)}
    "POST" {:status 200 :body (http/read-body req)}))

(with [server (http/server simple-server "127.0.0.1" 9816)]
  (test-http-item (http/request "GET" "http://127.0.0.1:9816") nil nil 200
                  {"content-length" "1"}))

(end-suite)
