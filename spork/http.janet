###
### http.janet
###
### Pure Janet HTTP/1.1 parser, client, and server.
###

(def- chunk-size (* 16 4096))

(defn- pre-pop
  "Remove n bytes from front of buffer"
  [buf n]
  (buffer/blit buf buf 0 n)
  (buffer/popn buf n))

(def- http-grammar
  ~{:request-status (* :method :ws :path :ws "HTTP/1." :d :any-ws :rn)
    :response-status (* "HTTP/1." :d :ws (/ ':d+ ,scan-number)
                        :ws '(any :printable) :rn)
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

(defn- accum-key-values
  "Accumulate key-value pairs based on arg index (even = key, odd = value) into a table and combine 
  duplicate keys into arrays of values (rather than overwriting). Used for both query strings and headers."
  [& args]
  (def tab @{})
  (loop [i :range [0 (length args) 2]
         :let [k (get args i) v (get args (+ 1 i))]]
    (if-let [item (in tab k)]
      (if (array? item)
        (array/push item v)
        (put tab k @[item v]))
      (put tab k v)))
  tab)

(defn- read-header
  "Read an HTTP header from a stream."
  [conn buf peg key1 key2]
  (var head nil)
  (var last-index 0)
  (forever
    (when-let [end (string/find "\r\n\r\n" buf last-index)]
      (set head
           (if-let [matches (peg/match peg buf)]
             (let [[a b] matches
                   headers (accum-key-values ;(array/remove matches 0 2))]
               @{:headers headers
                 :connection conn
                 :buffer (pre-pop buf (+ 4 end))
                 :head-size (+ 4 end)
                 key1 a
                 key2 b})
             :error))
      (break))
    (set last-index (max 0 (- (length buf) 4)))
    (unless (:read conn chunk-size buf)
      (set head :error)
      (break)))
  head)


(def query-string-grammar
  "Grammar that parses a query string (sans url path and ? character) and returns a table."
  (peg/compile
    ~{:qchar (+ (* "%" (/ (number (* :h :h) 16) ,string/from-bytes)) (* "+" (constant " ")))
      :kchar (+ :qchar (* (not (set "&=;")) '1))
      :vchar (+ :qchar (* (not (set "&;")) '1))
      :key (accumulate (some :kchar))
      :value (accumulate (any :vchar))
      :entry (* :key (+ (* "=" :value) (constant true)) (+ (set ";&") -1))
      :main (/ (any :entry) ,accum-key-values)}))

(defn read-request
  ``Read an HTTP request header from a connection. Returns a table with the following keys:
  * `:headers` - table mapping header names to header values. Header names are lowercase.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:head-size` - the number of bytes used by the header.
  * `:method` - the HTTP method used.
  * `:path` - the path of the resource requested.

  The following keys are also present, but omitted if the user passes a truthy parameter to `no-query`.
  * `:route` - path of the resource requested without query string.
  * `:query-string` - segment of HTTP path after first ? character.
  * `:query` - the query string parsed into a table. Supports a single string value
     for every string key, and any query parameters that aren't given a value are mapped to true.

  Note that data is read in chunks and any data after the header terminator is
  stored in `:buffer`.``
  [conn buf &opt no-query]
  (def head (read-header conn buf request-peg :method :path))
  (if (= :error head) (break head))

  # Parse query string separately
  (unless no-query
    (def fullpath (get head :path))
    (def qloc (string/find "?" fullpath))
    (def path (if qloc (string/slice fullpath 0 qloc) fullpath))
    (def qs (if qloc (string/slice fullpath (inc qloc)) nil))
    (put head :route path)
    (put head :query-string qs)
    (when qs
      (when-let [m (peg/match query-string-grammar qs)]
        (put head :query (first m)))))

  head)

(defn read-response
  ``Read an HTTP response header from a connection. Returns a table with the following keys:
  * `:headers` - table mapping header names to header values. Header names are lowercase.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:head-size` - the number of bytes used by the header.
  * `:status` - the HTTP status code.
  * `:message` - the HTTP status message.

  Note that data is read in chunks and any data after the header terminator is
  stored in `:buffer`.``
  [conn buf]
  (read-header conn buf response-peg :status :message))

(def status-messages
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
      (:write conn buf))

    (bytes? body)
    (do
      (buffer/format buf "Content-Length: %d\r\n\r\n%V" (length body) body)
      (:write conn buf))

    # default - iterate chunks
    (do
      (buffer/format buf "Transfer-Encoding: chunked\r\n\r\n")
      (each chunk body
        (assert (bytes? chunk) "expected byte chunk")
        (buffer/format buf "%x\r\n%V\r\n" (length chunk) chunk)
        (:write conn buf)
        (buffer/clear buf))
      (buffer/format buf "0\r\n\r\n")
      (:write conn buf)))
  (buffer/clear buf))

(defn- emit-chunk
  "Emit a chunk of response body to sink (function, buffer, file, or stream)."
  [sink chunk]
  (case (type sink)
    :nil nil
    :function (sink chunk)
    :buffer (buffer/push sink chunk)
    :core/file (file/write sink chunk)
    (unless (try (do (:write sink chunk) true) ([_] nil))
      (error (string "invalid sink type: " (type sink))))))

(def- chunk-header-peg
  (peg/compile
    ~(* (/ '(some :h) ,|(scan-number (string "16r" $)))
        (any (if-not "\r\n" 1))
        "\r\n"
        ($))))

(def- chunk-term-peg
  (peg/compile
    ~(* (+ "\r\n" (* (any (if-not "\r\n\r\n" 1)) "\r\n\r\n"))
        ($))))

(def- chunk-crlf-peg
  (peg/compile ~(* "\r\n" ($))))

(def- line-peg
  (peg/compile
    ~(* '(any (if-not (+ "\r\n" "\n") 1))
        (+ "\r\n" "\n")
        ($))))

(defn- read-until
  "Read single bytes from connection into buffer until the provided byte
  sequence is found within it. The buffer need not be empty. Returns the number
  of bytes from the start of the buffer until the substring."
  [conn buf needle &opt start-index]
  (default start-index 0)
  (when-let [pos (peg/find needle buf start-index)]
    (break pos))
  (prompt :exit
    (forever
      (unless (:read conn 1 buf)
        (error "end of stream"))
      (when-let [pos (peg/find needle buf start-index)]
        (return :exit pos)))))

(defn- drain-buf
  "Drain up to n bytes from src-buf into dest-buf. Returns number of bytes transferred."
  [dest-buf src-buf n]
  (if-let [_ (pos? (length src-buf))
           take (min n (length src-buf))]
    (do
      (buffer/blit dest-buf src-buf -1 0 take)
      (pre-pop src-buf take)
      take)
    0))

(defn- drain-raw-or-read
  "Drain available bytes from raw-buf or read from connection."
  [conn raw-buf n out-buf on-eof]
  (if-let [before (length out-buf)
           _ (or (pos? (drain-buf out-buf raw-buf n))
                 (:read conn n out-buf))]
    (- (length out-buf) before)
    (do (on-eof) 0)))

(defn- read-raw-payload
  "Read up to n decoded payload bytes into out-buf from the stream connection.
   Returns the number of bytes read, or 0 on EOF."
  [self n out-buf]
  (when (or (self :eof) (nil? (self :connection)))
    (break 0))
  (def {:mode mode
        :connection conn
        :buffer raw-buf} self)
  (case mode
    :none
    (do
      (set (self :eof) true)
      0)

    :fixed
    (if-let [rem (self :bytes-remaining)
             _ (pos? rem)
             got (drain-raw-or-read conn raw-buf (min n rem) out-buf
                                    |(error (string "premature end of stream: expected " rem " more bytes")))]
      (do
        (-= (self :bytes-remaining) got)
        (when (<= (self :bytes-remaining) 0)
          (set (self :eof) true))
        got)
      (do
        (set (self :eof) true)
        0))

    :identity
    (drain-raw-or-read conn raw-buf n out-buf |(set (self :eof) true))

    :event-stream
    (drain-raw-or-read conn raw-buf n out-buf |(set (self :eof) true))

    :chunked
    (let [before (length out-buf)]
      (var needed n)
      (while (and (pos? needed) (not (self :eof)))
        (case (self :chunk-state)
          :header
          (do
            (while (and (not (peg/match chunk-header-peg raw-buf)) (not (self :eof)))
              (unless (:read conn chunk-size raw-buf)
                (error "unexpected end of stream reading chunk header")))
            (match (peg/match chunk-header-peg raw-buf)
              [0 pop-len]
              (do
                (pre-pop raw-buf pop-len)
                (while (and (not (peg/match chunk-term-peg raw-buf)) (not (self :eof)))
                  (unless (:read conn 2 raw-buf)
                    (break)))
                (match (peg/match chunk-term-peg raw-buf)
                  [term-len] (pre-pop raw-buf term-len))
                (put self :chunk-state :done)
                (set (self :eof) true))

              [chunk-len pop-len]
              (do
                (pre-pop raw-buf pop-len)
                (put self :chunk-remaining chunk-len)
                (put self :chunk-state :payload))

              _
              (set (self :eof) true)))

          :payload
          (when-let [take (min needed (self :chunk-remaining))
                     got (drain-raw-or-read conn raw-buf take out-buf
                                            |(error "unexpected end of stream reading chunk payload"))
                     _ (-= (self :chunk-remaining) got)
                     _ (-= needed got)
                     _ (zero? (self :chunk-remaining))]
            (put self :chunk-state :crlf))

          :crlf
          (do
            (while (and (not (peg/match chunk-crlf-peg raw-buf)) (not (self :eof)))
              (unless (:read conn 2 raw-buf)
                (error "unexpected end of stream reading chunk trailing CRLF")))
            (match (peg/match chunk-crlf-peg raw-buf)
              [crlf-len]
              (do
                (pre-pop raw-buf crlf-len)
                (put self :chunk-state :header))

              _
              (set (self :eof) true)))))
      (- (length out-buf) before))

    0))

(defn- emit-buf
  "Append val to user-buf if provided, or return val directly."
  [user-buf val]
  (if user-buf (buffer/push user-buf val) val))

(defn- stream-read
  "Read from response stream:
   - If n-or-what is integer: reads up to n bytes
   - If n-or-what is :all or nil: reads to EOF
   - If n-or-what is :line: reads up to newline
   Appends to optional user-buf or allocates a new buffer. Returns buffer or nil on EOF."
  [self &opt n-or-what user-buf]
  (default n-or-what :all)
  (def payload-buf (self :payload-buf))
  (match n-or-what
    :line
    (do
      (while (and (not (peg/match line-peg payload-buf))
                  (not (self :eof))
                  (pos? (read-raw-payload self chunk-size payload-buf))))
      (match (peg/match line-peg payload-buf)
        [matched-line pop-len]
        (do
          (pre-pop payload-buf pop-len)
          (emit-buf user-buf matched-line))

        _
        (when-let [_ (pos? (length payload-buf))
                   rest-line (string payload-buf)
                   _ (buffer/clear payload-buf)]
          (emit-buf user-buf rest-line))))

    :all
    (let [out (or user-buf @"")]
      (drain-buf out payload-buf (length payload-buf))
      (while (and (not (self :eof))
                  (pos? (read-raw-payload self chunk-size out))))
      out)

    (n (and (number? n) (<= n 0)))
    (or user-buf @"")

    (n (number? n))
    (when-let [out (or user-buf (buffer/new n))
               from-buf (drain-buf out payload-buf n)
               needed (- n from-buf)
               got-net (and (pos? needed)
                            (not (self :eof))
                            (read-raw-payload self needed out))
               _ (or (pos? from-buf) (and got-net (pos? got-net)))]
      out)

    _
    (error (string "invalid read argument: " n-or-what))))

(defn make-response-stream
  "Create a streaming HTTP response object from a connection and parsed header."
  [conn head &opt url method]
  (default url "")
  (default method "GET")
  (def headers (head :headers))
  (def no-body? (or (= method "HEAD")
                    (= (head :status) 204)
                    (= (head :status) 304)))
  (def mode
    (cond
      no-body? :none
      (= (get headers "transfer-encoding") "chunked") :chunked
      (in headers "content-length") :fixed
      (-?>> (in headers "content-type") (string/has-prefix? "text/event-stream")) :event-stream
      conn :identity
      :none))
  (def cl (when-let [v (in headers "content-length")]
            (scan-number v)))
  @{:status (head :status)
    :message (head :message)
    :headers headers
    :head-size (head :head-size)
    :url url
    :method method
    :connection conn
    :mode mode
    :buffer (or (head :buffer) @"")
    :payload-buf @""
    :bytes-remaining (or cl 0)
    :chunk-remaining 0
    :chunk-state :header
    :eof no-body?
    :read (fn [self &opt what buf] (stream-read self what buf))
    :blocks (fn [self &opt block-size]
              (coro
                (def bsz (or block-size chunk-size))
                (forever
                  (if-let [b (:read self bsz (buffer/new bsz))]
                    (yield (buffer/slice b))
                    (break)))))
    :lines (fn [self &named separator]
             (coro
               (if (or (nil? separator) (= separator "\n"))
                 (forever
                   (if-let [line (:read self :line)]
                     (yield line)
                     (break)))
                 (let [custom-peg (peg/compile ~(* '(any (if-not ,separator 1)) ,separator ($)))
                       payload-buf (self :payload-buf)]
                   (forever
                     (while (and (not (peg/match custom-peg payload-buf))
                                 (not (self :eof))
                                 (pos? (read-raw-payload self chunk-size payload-buf))))
                     (match (peg/match custom-peg payload-buf)
                       [line pop-len]
                       (do
                         (pre-pop payload-buf pop-len)
                         (yield line))

                       _
                       (do
                         (when (pos? (length payload-buf))
                           (yield (string payload-buf))
                           (buffer/clear payload-buf))
                         (break))))))))
    :close (fn [self]
             (when-let [c (in self :connection)]
               (put self :connection nil)
               (try (:close c) ([_] nil))))})

(defn read-body
  ``Given a request/response table, read the HTTP body from the connection.
  If sink is provided (or (in req :sink)), streams chunks to sink (function,
  file, buffer, or stream). Otherwise returns the body as a buffer. If the request
  has no body, returns nil.``
  [req &opt sink]
  (when-let [body (in req :body)]
    (break body))
  (def stream (make-response-stream (in req :connection) req "" (in req :method "")))
  (if-let [target-sink (or sink (in req :sink))]
    (do
      (loop [b :in (:blocks stream)]
        (emit-chunk target-sink b))
      (when (buffer? target-sink)
        (set (req :body) target-sink))
      target-sink)
    (set (req :body) (:read stream :all))))

(defn send-response
  ``Send an HTTP response over a connection. Will automatically use chunked
  encoding if body is not a byte sequence. `response` should be a table
  with the following keys:

  * `:headers` - optional headers to write
  * `:status` - integer status code to write
  * `:body` - optional byte sequence or iterable (for chunked body)
     for returning contents. The iterable can be lazy, i.e. for streaming
     data.``
  [conn response &opt buf]
  (default buf @"")
  (def status (get response :status 200))
  (def message (in status-messages status))
  (buffer/format buf "HTTP/1.1 %d %s\r\n" status message)
  (def headers (get response :headers {}))

  (eachp [k v] headers
    # Values can be lists when representing duplicate headers (e.g.: multiple "Set-Cookie" entries)
    (if (or (tuple? v) (array? v))
      (each ve v (buffer/format buf "%V: %V\r\n" k ve))
      (buffer/format buf "%V: %V\r\n" k v)))

  (write-body conn buf (in response :body)))

###
### Server Middleware
###

(defn- bytes-to-mw
  [b]
  (fn mw [&] {:status 200 :body b}))

(defn middleware
  "Coerce any type to http middleware"
  [x]
  (case (type x)
    :function x
    :number (let [msg (get status-messages x)]
              (assert msg (string "unknown http status code when making middleware: " x))
              (fn mw [&] {:status x :body msg}))
    :string (bytes-to-mw x)
    :buffer (bytes-to-mw x)
    (fn mw [&] x)))

(defn router
  "Creates a router middleware. A router will dispatch to different routes based on
  the URL path."
  [routes]
  (fn router-mw [req]
    (if-let [r (or (get routes (get req :route))
                   (get routes :default))]
      ((middleware r) req)
      {:status 404 :body "Not Found"})))

(defn logger
  "Creates a logging middleware. The logger middleware prints URL route, return status, and elapsed request time."
  [nextmw]
  (def f (dyn *out* stdout))
  (fn logger-mw [req]
    (def {:path path
          :method method} req)
    (def start-clock (os/clock))
    (def ret (nextmw req))
    (def end-clock (os/clock))
    (def elapsed (string/format "%.3f" (* 1000 (- end-clock start-clock))))
    (def status (or (get ret :status) 200))
    (xprint f method " " status " " path " elapsed " elapsed "ms")
    (file/flush f)
    ret))

(def cookie-grammar
  "Grammar to parse a cookie header to a series of keys and values."
  (peg/compile
    {:content '(some (if-not (set "=;") 1))
     :eql "="
     :sep '(between 1 2 (set "; "))
     :main '(some (* (<- :content) :eql (<- :content) (? :sep)))}))

(defn cookies
  "Parses cookies into the table under :cookies key"
  [nextmw]
  (fn cookie-mw [req]
    (-> req
        (put :cookies
             (or (-?>> [:headers "cookie"]
                       (get-in req)
                       (peg/match cookie-grammar)
                       (apply table))
                 {}))
        nextmw)))

###
### Server boilerplate
###

(defn server-handler
  ``A simple connection handler for an HTTP server.
  When a connection is accepted. Call this with a handler
  function to handle the connect. The handler will be called
  with one argument, the request table, which will contain the
  following keys:
  * `:head-size` - number of bytes in the http header.
  * `:headers` - table mapping header names to header values.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:path` - HTTP path.
  * `:method` - HTTP method, as a string.``
  [conn handler]
  (def handler-mw (middleware handler))
  (defer (:close conn)

    # Get request header
    (def buf (buffer/new chunk-size))
    (def req (read-request conn buf))

    # Handle bad request
    (when (= :error req)
      (send-response conn {:status 400} (buffer/clear buf))
      (break))

    # Add some extra keys to the request
    (put req :connection conn)

    # Do something with request header
    (def response (handler-mw req))

    # Now send back response
    (send-response conn response @"")))

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
  ~{:main (* (+ :https :http) :fqdn :port :path)
    :https (* (constant "https") "https://")
    :http (* (constant "http") "http://")
    :fqdn '(some (range "az" "AZ" "09" ".." "--"))
    :port (+ (* ":" ':d+) (constant nil))
    :path-chr (range "az" "AZ" "09" "!!" "$9" ":;" "==" "?@" "~~" "__")
    :path (+ '(some :path-chr) (constant "/"))})

(def url-grammar
  "Grammar to parse a URL into scheme, domain, port, and path. Supports
  both http:// and https:// protocols. Returns [scheme host port path]."
  (peg/compile url-peg-source))

(defn resolve-url
  "Resolve a redirect location against a base URL per RFC 9110."
  [base-url location]
  (cond
    (or (string/has-prefix? "http://" location)
        (string/has-prefix? "https://" location))
    location

    (if-let [[scheme host raw-port path] (peg/match url-grammar base-url)
             origin (string scheme "://" host (if raw-port (string ":" raw-port) ""))
             p (or path "/")
             idx (last (string/find-all "/" p))
             prefix (cond
                      (string/has-prefix? "/" location) ""
                      idx (string/slice p 0 (inc idx))
                      "/")]
      (string origin prefix location)
      (error (string "invalid base url: " base-url)))))

(defn open-stream
  ``Open an HTTP request and return an open response stream.
  The stream implements :status, :message, :headers, :url, :head-size,
  :read, :blocks, :lines, and :close.
  Automatically follows HTTP 3xx redirects up to :max-redirects (default 10).

  Options:
  * `:method` - HTTP method string (default "GET")
  * `:body` - Request body content
  * `:headers` - Request headers table
  * `:stream-factory` - Function to create connection stream. Defaults to net/connect.
  * `:stream-opts` - Options table passed to stream-factory
  * `:max-redirects` - Maximum number of 3xx redirects to follow (default 10)``
  [url &keys
   {:method method
    :body body
    :headers headers
    :stream-factory stream-factory
    :stream-opts stream-opts
    :max-redirects max-redirects}]
  (default method "GET")
  (default max-redirects 10)
  (def [scheme host raw-port path]
    (or (peg/match url-grammar url)
        (error (string "invalid url: " url))))
  (def port (or raw-port (if (= scheme "https") "443" "80")))
  (def target-path (or path "/"))
  (def buf @"")
  (buffer/format buf "%s %s HTTP/1.1\r\nHost: %s:%s\r\n" method target-path host port)
  (when headers
    (eachp [k v] headers
      (buffer/format buf "%s: %s\r\n" k v)))

  (def effective-opts (merge (or stream-opts {}) {:scheme scheme}))
  (def conn (if stream-factory
              (stream-factory host port effective-opts)
              (net/connect host port)))

  # Write request body
  (write-body conn buf body)

  # Read response header
  (match (read-response conn buf)
    :error
    (do
      (try (:close conn) ([_] nil))
      (error "failed to read HTTP response header"))

    head
    (if-let [_ (pos? max-redirects)
             loc (get (head :headers) "location")
             status (head :status)
             _ (find |(= status $) [301 302 303 307 308])]
      (do
        (try (:close conn) ([_] nil))
        (let [get? (find |(= status $) [301 302 303])]
          (open-stream (resolve-url url loc)
                       :method (if get? "GET" method)
                       :body (unless get? body)
                       :headers headers
                       :stream-factory stream-factory
                       :stream-opts stream-opts
                       :max-redirects (dec max-redirects))))
      (make-response-stream conn head url method))))

(defn request
  ``Make an HTTP request to a server.
  Returns a table containing response information:
  * `:head-size` - number of bytes in the http header
  * `:headers` - table mapping header names to header values. Header names are lowercase.
  * `:connection` - the connection stream for the header.
  * `:buffer` - the buffer instance that may contain extra bytes.
  * `:status` - HTTP status code as an integer.
  * `:message` - HTTP status message.
  * `:url` - final resolved URL.
  * `:body` - Bytes of the response body (nil if streaming to custom sink or method is HEAD).

  Options:
  * `:body` - Request body content
  * `:headers` - Request headers table
  * `:stream-factory` - Function to create connection stream. Defaults to net/connect.
    Signature: (stream-factory host port stream-opts)
  * `:stream-opts` - Options table passed to stream-factory
  * `:sink` - Target to stream response body into (function, file, or buffer)
  * `:stream` - If true, returns the open response stream directly without consuming body
  * `:max-redirects` - Maximum number of 3xx redirects to follow (default 0)``
  [method url &keys opts]
  (def req-opts (merge {:max-redirects 0} opts))
  (if (get opts :stream)
    (open-stream url :method method ;(kvs req-opts))
    (with [stream (open-stream url :method method ;(kvs req-opts))]
      (when-let [sink (in opts :sink)]
        (loop [b :in (:blocks stream)]
          (emit-chunk sink b)))
      @{:head-size (stream :head-size)
        :headers (stream :headers)
        :connection (stream :connection)
        :buffer (stream :buffer)
        :status (stream :status)
        :message (stream :message)
        :url (stream :url)
        :body (unless (or (in opts :sink) (= method "HEAD"))
                (:read stream :all))})))

(defn download
  ``Download an HTTP resource to a destination path, file, buffer, or sink.
  Supports automatic redirect following, chunked transfer encoding,
  and incremental chunk streaming without buffering entire responses.

  Arguments:
  * `url` - The URL to download
  * `dest` - Target file path (string), buffer, core/file or stream, or sink function (fn [chunk])

  Options:
  * `:headers` - Table of HTTP headers
  * `:max-redirects` - Maximum redirects to follow (default 10)
  * `:stream-factory` - Custom stream factory function
  * `:stream-opts` - Options passed to stream-factory``
  [url dest &keys opts]
  (with [stream (open-stream url ;(kvs (merge {:max-redirects 10} opts)))]
    (cond
      (<= 200 (stream :status) 299) nil
      (and (find |(= (stream :status) $) [301 302 303 307 308]) (get (stream :headers) "location"))
      (error (string "Too many redirects: exceeded limit with status " (stream :status)))
      (error (string "HTTP download failed with status " (stream :status) ": " (stream :message))))
    (case (type dest)
      :string
      (if-let [f (file/open dest :wb)]
        (defer (file/close f)
          (loop [b :in (:blocks stream)]
            (file/write f b)))
        (error (string "failed to open destination file: " dest)))

      :buffer
      (:read stream :all dest)

      (loop [b :in (:blocks stream)]
        (emit-chunk dest b)))
    stream))
