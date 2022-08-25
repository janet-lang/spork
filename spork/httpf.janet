###
### httpf.janet
###
### A simple HTTP framework for HTML, JDN, and JSON servers.
###

(import ./ev-utils)
(import ./htmlgen)
(import ./http)
(import ./schema)

(import spork/json)

# Parse the Accept header and find the first support mime type.
(def- accept-peg
  (peg/compile
    ~{:main (+
             (* '"text/html" (+ -1 ","))
             (* '"application/json" (+ -1 ","))
             (* '"application/jdn" (* -1 ","))
             -1
             (* (thru ",") :main))}))

(defn- render-json
  [data buf]
  (json/encode data "  " "\n" buf)
  (buffer/push buf "\n"))

(defn- render-jdn
  [data buf]
  (buffer/format buf "%j\n" data))

(defn- render-html
  [data buf]
  (def data (or (get data :data) data))
  (if (dictionary? data)
    (do
      (buffer/push buf "<pre><code>")
      (json/encode data "  " "\n" buf)
      (buffer/push buf "</code></pre>")
      buf)
    (htmlgen/html data buf)))

(def- render-map
  {"text/html" render-html
   "application/json" render-json
   "application/jdn" render-jdn})

(defn- read-json
  [body]
  (json/decode body))

(defn- read-jdn
  [body]
  (parse body))

(def- reader-map
  {"application/json" read-json
   "application/jdn" read-jdn})

(defn- make-schema-rep
  "Convert a nested object to an approximation that can
  be converted to JSON or jdn. This means no functions, cycles,
  or other objects besides primitives and built in datastructures."
  [x]
  (case (type x)
    :nil x
    :boolean x
    :number x
    :string x
    :keyword x
    :symbol x
    :array (map make-schema-rep x)
    :tuple (tuple/slice (map make-schema-rep x))
    :table (let [t @{}]
             (eachp [k v] x (put t (make-schema-rep k) (make-schema-rep v)))
             t)
    :struct (let [t @{}]
             (eachp [k v] x (put t (make-schema-rep k) (make-schema-rep v)))
             (table/to-struct t))
    (describe x)))

(defn add-route
  "Add a route to a server"
  [server path docstring schema handler]
  (def routes (get server :routes))
  (def docs (get server :route-docs))
  (def schemas (get server :schemas))
  (def schema-sources (get server :schema-sources))
  (when (get routes path)
    (errorf "duplicate routes for " path))
  (put docs path docstring)
  (put routes path handler)
  (put schemas path (schema/make-validator schema))
  (put schema-sources path (make-schema-rep schema))
  (if (string/has-suffix? "/" path)
    (add-route server (string/slice path 0 -2) docstring schema handler)
    server))

(defn- add-bindings
  "Add all local functions defined with :path metadata to a server. Will
  read from the :schema, :doc, :path, and :route-doc metadata to determine how
  the route behaves."
  [server &opt env]
  (default env (curenv))
  (loop [[binding meta] :pairs env
         :when (symbol? binding)
         :let [path (get meta :path)
               schema (get meta :schema)
               value (get meta :value)
               docstring (get meta :route-doc (get meta :doc))]
         :when value
         :when (bytes? path)]
    (add-route server path docstring schema value))
  server)

(defn default-payload-wrapper
  "Add some metadata to all responses"
  [payload]
  {:data payload
   :janet-version janet/version
   :janet-build janet/build
   :os (os/which)
   :time (os/time)})

(defn server
  "Create a new server."
  [&opt host port route-table]
  (def routes @{})
  (def schemas @{})
  (def schema-sources @{})
  (def route-docs @{})
  (def wrapper default-payload-wrapper)
  (def state @{:routes routes
               :schemas schemas
               :schema-sources schema-sources
               :route-docs route-docs})

  (defn- generic-handler
    [req]
    (def path (get req :path))
    (def method (get req :method))
    (def headers (get req :headers))
    (def reader-mime (get headers "content-type" "application/json"))
    (def accepts (get headers "accept" reader-mime))
    (def render-mime
      (if-let [res (peg/match accept-peg accepts)]
        (in res 0)
        reader-mime))
    (def reader (get reader-map reader-mime))
    (def render (get render-map render-mime))
    (def handler (get routes path))

    (defn make-response
      [code content]
      {:status code
       :headers {"content-type" render-mime
                 "server" "spork/httpf"}
       :body (render (wrapper content) @"")})

    # Check for bad mime types
    (unless reader
      (break
        (make-response 400 (string "cannot read payload with mimetype " accepts))))

    # Check not found
    (unless handler (break (make-response 404 "not found")))

    (try
      (case method
        "GET" (try
                (make-response
                  200
                  (do
                    (if-let [validate (get schemas path)]
                      (validate nil)) (handler req)))
                ([err] (make-response 400 (string err))))
        "OPTIONS" {:status 200
                   :headers {"allow" "OPTIONS, GET, POST"
                             "content-type" render-mime
                             "server" "spork/httpf"}
                   :body (render (wrapper {:doc (get route-docs path "no docs found")
                                           :schema (get schema-sources path)}) @"")}
        "POST" (let [validate (get schemas path)
                     body (http/read-body req)
                     data (if body (reader body))]
                   (try
                     (make-response 200 (do (if validate (validate data)) (handler req data)))
                     ([err] (make-response 400 (string err)))))
        (make-response 405 "Method not allowed. Use GET, OPTIONS, or POST."))
    ([err]
     (make-response 500 err))))

  (put state :on-connection
       (fn [conn]
         (http/server-handler
           conn
           (-> generic-handler
               http/cookies
               http/logger))))

  (add-bindings state route-table)

  state)

(defn listen
  "Start server. Will run on multiple-threads if n-workers > 1."
  [server &opt n-workers host port]
  (default host "0.0.0.0")
  (default port "8000")
  (default n-workers 1)

  (def on-connection (get server :on-connection))

  (if (> n-workers 1)
    (do
      (defn thread-main
        [tid &]
        (def s (net/listen host port))
        (printf "thread %V server listening on %V:%V..." tid host port)
        (net/accept-loop s on-connection)
        (print "server closed on thread " tid))
      (ev-utils/multithread-service thread-main n-workers))
    (do
      (printf "listening on %V:%V..." host port)
      (def s (net/listen host port))
      (net/accept-loop s on-connection)
      (print "server closed"))))

