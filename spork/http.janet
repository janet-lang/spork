###
### http.janet
###
### Pure Janet HTTP/1.1 parser, client, and server.
###

# Handle max read size bug.
(def- chunk-size 4096)

(defn- pre-pop
  "Remove n bytes from front of buffer"
  [buf n]
  (buffer/blit buf buf 0 n)
  (buffer/popn buf n))

(def- http-grammar
  ~{:request-status (* :method :ws :path :ws "HTTP/1." :d :any-ws :rn)
    :response-status (* "HTTP/1." :d :ws (/ ':d+ ,scan-number)
                        :ws '(some :printable) :rn)
    :ws (some (set " \t"))
    :any-ws (any (set " \t"))
    :rn "\r\n"
    :method '(some (range "AZ"))
    :path-chr (range "az" "AZ" "09" "!!" "$9" ":;" "==" "?@" "~~" "__")
    :path '(some :path-chr)
    :printable (range "\x20~" "\t\t")
    :headers (* (any :header) :rn)
    # lower case header names since http headers are case-insensitive
    :header-name (/ '(some (range "\x219" ";~")) ,string/ascii-lower)
    :header-value '(any :printable)
    :header (* :header-name ":" :any-ws :header-value :rn)})

(def request-peg
  "PEG for parsing HTTP requests"
  (peg/compile
    (table/to-struct
      (merge {:main ~(* :request-status :headers)}
             http-grammar))))

(def response-peg
  "PEG for parsing HTTP responses"
  (peg/compile
    (table/to-struct (merge
                       {:main ~(* :response-status :headers)}
                       http-grammar))))

(defn read-http-peg
  "Read from a stream until the HTTP header terminator, and
  then parse the buffer with a peg."
  [conn buf peg key1 key2]
  (var head nil)
  (var last-index 0)
  (forever
    (when-let [end (string/find "\r\n\r\n" buf last-index)]
      (set head
           (if-let [matches (peg/match peg buf)]
             (let [[a b] matches
                   headers (table ;(array/remove matches 0 2))]
               @{:headers headers
                 :connection conn
                 :buffer buf
                 :head-size (+ 4 end)
                 key1 a
                 key2 b})
             :error))
      (break))
    (set last-index (length buf))
    (ev/read conn chunk-size buf))
  head)

(defn read-request
  "Read an HTTP request header from a connection"
  [conn buf]
  (read-http-peg conn buf request-peg :method :path))

(defn read-response
  "Read an HTTP response header from a connection"
  [conn buf]
  (read-http-peg conn buf response-peg :status :message))

(def http-status-messages
  "Mapping of HTTP status codes to their status message."
  {100 "Continue"
   101 "Switching Protocols"
   102 "Processing"
   200 "OK"
   201 "Created"
   202 "Accepted"
   203 "Non-Authoritative Information"
   204 "No Content"
   205 "Reset Content"
   206 "Partial Content"
   207 "Multi-Status"
   208 "Already Reported"
   226 "IM Used"
   300 "Multiple Choices"
   301 "Moved Permanently"
   302 "Found"
   303 "See Other"
   304 "Not Modified"
   305 "Use Proxy"
   307 "Temporary Redirect"
   308 "Permanent Redirect"
   400 "Bad Request"
   401 "Unauthorized"
   402 "Payment Required"
   403 "Forbidden"
   404 "Not Found"
   405 "Method Not Allowed"
   406 "Not Acceptable"
   407 "Proxy Authentication Required"
   408 "Request Timeout"
   409 "Conflict"
   410 "Gone"
   411 "Length Required"
   412 "Precondition Failed"
   413 "Payload Too Large"
   414 "URI Too Long"
   415 "Unsupported Media Type"
   416 "Range Not Satisfiable"
   417 "Expectation Failed"
   421 "Misdirected Request"
   422 "Unprocessable Entity"
   423 "Locked"
   424 "Failed Dependency"
   426 "Upgrade Required"
   428 "Precondition Required"
   429 "Too Many Requests"
   431 "Request Header Fields Too Large"
   451 "Unavailable For Legal Reasons"
   500 "Internal Server Error"
   501 "Not Implemented"
   502 "Bad Gateway"
   503 "Service Unavailable"
   504 "Gateway Timeout"
   505 "HTTP Version Not Supported"
   506 "Variant Also Negotiates"
   507 "Insufficient Storage"
   508 "Loop Detected"
   510 "Not Extended"
   511 "Network Authentication Required"})

(defn- write-body
  "Write the body of an HTTP request, adding Content-Length header
  or Transfer-Encoding: chunked"
  [conn buf body]
  (cond
    (nil? body)
    (do
      (buffer/push buf "\r\n")
      (ev/write conn buf))

    (bytes? body)
    (do
      (buffer/format buf "Content-Length: %d\r\n\r\n%s" (length body) body)
      (ev/write conn buf))

    # default - iterate chunks
    (do
      (buffer/format buf "Transfer-Encoding: chunked\r\n\r\n")
      (each chunk body
        (buffer/format buf "%d\r\n%s\r\n" (length chunk) chunk)
        (ev/write conn buf)
        (buffer/clear buf))
      (buffer/format buf "0\r\n\r\n")
      (ev/write conn buf)))
  (buffer/clear buf))

(defn read-body
  "Given a request, read the HTTP body from the connection. Returns the body as a buffer.
  If the request has no body, returns nil."
  [req]
  (when-let [body (in req :body)] (break body))
  (def headers (in req :headers))

  # In place content
  (when-let [cl (in headers "content-length")]
    (def {:buffer buf
          :connection conn
          :head-size head-size} req)
    (def content-length (scan-number cl))
    (def remaining (- content-length (- (length buf) head-size)))
    (when (pos? remaining)
      (ev/chunk conn remaining buf))
    (put req :body (buffer/slice buf head-size))
    (break buf))

  # TODO - Chunked encoding

  # no body, just return nil
  nil)

(defn send-response
  "Send an HTTP response over a connection. Will automatically use chunked
  encoding if body is not a byte sequence. `response` should be a table
  with the following keys:

  * `:headers` - optional headers to write
  * `:status` - integer status code to write
  * `:body` - optional byte sequence or iterable (for chunked body)
     for returning contents. The iterable can be lazy, i.e. for streaming
     data."
  [conn response &opt buf]
  (default buf @"")
  (def status (get response :status 200))
  (def message (in http-status-messages status))
  (buffer/format buf "HTTP/1.1 %d %s\r\n" status message)
  (def headers (get response :headers {}))
  (eachp [k v] headers
    (buffer/format buf "%s: %s\r\n" k v))
  (write-body conn buf (in response :body)))

(defn server-handler
  "A simple connection handler for an HTTP server.
  When a connection is accepted. Call this with a handler
  function to handle the connect. The handler will be called
  with one argument, the request table, which will contain the
  following keys:
  * `:head-size` - number of bytes in the http header.
  * `:headers` - table mapping header names to header values.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:path` - HTTP path.
  * `:method` - HTTP method, as a string."
  [conn handler]
  (defer (ev/close conn)

    # Get request header
    (def buf (buffer/new chunk-size))
    (def req (read-request conn buf))

    # Handle bad request
    (when (= :error req)
      (send-response conn {:status 400} (buffer/clear buf))
      (break))

    # Add some extra keys to the request
    (put req :connection conn)
    (pre-pop buf (in req :head-size))

    # Do something with request header
    (def response (handler req))

    # Now send back response
    (send-response conn response (buffer/clear buf))))

(defn server
  "Makes a simple http server. By default it binds to 0.0.0.0:8000,
  returns a new server stream.
  Simply wraps http/server-handler with a net/server."
  [handler &opt host port]
  (default host "0.0.0.0")
  (default port 8000)
  (defn new-handler
    [conn]
    (server-handler conn handler))
  (net/server host port new-handler))

###
### HTTP Client
###

(def- url-peg-source
  ~{:main (* :protocol :fqdn :port :path)
    :protocol "http://" # currently no https support
    :fqdn '(some (range "az" "AZ" "09" ".." "--"))
    :port (+ (* ":" ':d+) (constant 80))
    :path-chr (range "az" "AZ" "09" "!!" "$9" ":;" "==" "?@" "~~" "__")
    :path (+ '(some :path-chr) (constant "/"))})

(def- url-peg (peg/compile url-peg-source))

(defn request
  "Make an HTTP request to a server.
  Returns a table contain response information.
  * `:head-size` - number of bytes in the http header
  * `:headers` - table mapping header names to header values. Header names are lowercase.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:status` - HTTP status code as an integer.
  * `:message` - HTTP status message.
  * `:body` - Bytes of the response body."
  [method url &keys
   {:body body
    :headers headers}]
  (def x (peg/match url-peg url))
  (assert x (string "invalid url: " url))
  (def [host port path] x)
  (def buf @"")
  (buffer/format buf "%s %s HTTP/1.1\r\nHost: %s:%v\r\n" method path host port)
  (when headers
    (eachp [k v] headers
      (buffer/format buf "%s: %s\r\n" k v)))
  (with [conn (net/connect host port)]

    # Make request
    (write-body conn buf body)

    # Parse response pure janet
    (def res (read-response conn buf))
    (read-body res)
    (when (= :error res) (error res))

    # TODO - handle redirects with Location header
    res))
