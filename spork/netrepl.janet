###
### netrepl.janet
###
### A simple async networked repl (both client and server) with a remote debugger
### and the ability to repl into existing environments.
###

(use ./msg)

(def default-host
  "Default host to run server on and connect to."
  "127.0.0.1")

(def default-port
  "Default port to run the net repl."
  "9365")

# Specifying the Environment
#
# Provide various ways to produce the environment to repl into.
# 1. an environment factory function, called for each connection.
# 2. an env (table value) - this means every connection will share the
#    same environment
# 3. default env, made via make-env with nice printing for each new connection.

(defn- coerce-to-env
  "Get an environment for the repl."
  [env name stream]
  (cond
    (function? env) (env name stream)
    (not= nil env) env
    (let [e (make-env)]
      (put e :pretty-format "%.20M"))))

# NETREPL Protocol
#
# Clients don't need to support steps 4. and 5. if they never send messages prefixed
# with 0xFF or 0xFE bytes. These bytes should not occur in normal Janet source code and
# are not even valid utf8.
#
# 1. server <- {user specified name of client (will be shown in repl)} <- client
# 2. server -> {repl prompt (no newline)} -> client
# 3. server <- {one chunk of input (msg)} <- client
# 4. If (= (msg 0) 0xFF)
#   4a. (def result (-> msg (slice 1) parse eval protect))
#   4b. server -> result -> client
#   4c. goto 3.
# 5. If (= (msg 0) 0xFE)
#   5a. Return msg as either:
#       i. a keyword if the msg contains a command (e.g. :cancel)
#       ii. an array if the msg contains a command and arguments (e.g. @[:source "path/to/source"]
#   5b. goto 6b.
# 6. Otherwise
#   6a. Send chunk to repl input stream
#   6b. server -> {(dyn :out) and (dyn :err) (empty at first)} -> client
#   6c. goto 2.

(defn- make-onsignal
  "Make an onsignal handler for debugging. Since the built-in repl
  calls getline which blocks, we use our own debugging functionality."
  [getter env e level]
  (defn enter-debugger
    [f x]
    (def nextenv (make-env env))
    (put nextenv :fiber f)
    (put nextenv :debug-level level)
    (put nextenv :signal x)
    (merge-into nextenv debugger-env)
    (debug/stacktrace f x)
    (eflush)
    (defn debugger-chunks [buf p]
      (def status (parser/state p :delimiters))
      (def c ((parser/where p) 0))
      (def prpt (string "debug[" level "]:" c ":" status "> "))
      (getter prpt buf))
    (print "entering debug[" level "] - (quit) to exit")
    (flush)
    (repl debugger-chunks (make-onsignal getter env nextenv (+ 1 level)) nextenv)
    (print "exiting debug[" level "]")
    (flush)
    (nextenv :resume-value))
  (fn [f x]
    (if (= :dead (fiber/status f))
      (do (put e '_ @{:value x}) (pp x))
      (if (e :debug)
        (enter-debugger f x)
        (do (debug/stacktrace f x) (eflush))))))

(defn server
  "Start a repl server. The default host is \"127.0.0.1\" and the default port
  is \"9365\". Calling this will start a TCP server that exposes a
  repl into the given env. If no env is provided, a new env will be created
  per connection. If env is a function, that function will be invoked with
  the name and stream on each connection to generate an environment."
  [&opt host port env]
  (default host default-host)
  (default port default-port)
  (print "Starting networked repl server on " host ", port " port "...")
  (net/server
    host port
    (fn repl-handler [stream]
      (var name "<unknown>")
      (def outbuf @"")
      (defn wrapio [f] (fn [& a] (with-dyns [:out outbuf :err outbuf] (f ;a))))
      (defer (:close stream)
        (def recv (make-recv stream))
        (def send (make-send stream))
        (set name (or (recv) (break)))
        (print "client " name " connected")
        (def e (coerce-to-env env name stream))
        (def p (parser/new))
        (def cmd-g
          ~{:main (* :command (any (* :space :argument)))
            :space (some (set " \t"))
            :identifier (some :S)
            :command (/ ':identifier ,keyword)
            :argument (/ '(+ :quoted-arg :bare-arg) ,parse)
            :bare-arg :identifier
            :quoted-arg (* `"` (any (+ (* `\` 1) (if-not `"` 1))) `"`)})
        (var is-first true)
        (defn getline-async
          [prmpt buf]
          (if is-first
            (set is-first false)
            (do
              (send outbuf)
              (buffer/clear outbuf)))
          (send prmpt)
          (var ret nil)
          (while (def msg (recv))
            (cond
              (= 0xFF (in msg 0))
              (send (string/format "%j" (-> msg (slice 1) parse eval protect)))
              (= 0xFE (in msg 0))
              (do
                (def cmd (peg/match cmd-g msg 1))
                (if (one? (length cmd))
                  (set ret (first cmd))
                  (set ret cmd))
                (break))
              (do (buffer/push-string buf msg) (break))))
          ret)
        (defn chunk
          [buf p]
          (def delim (parser/state p :delimiters))
          (def lno ((parser/where p) 0))
          (getline-async (string name ":" lno ":" delim  " ") buf))
        (->
          (run-context
            {:env e
             :chunks chunk
             :on-status (make-onsignal getline-async e e 1)
             :on-compile-error (wrapio bad-compile)
             :on-parse-error (wrapio bad-parse)
             :evaluator (fn [x &] (setdyn :out outbuf) (setdyn :err outbuf) (x))
             :source "repl"
             :parser p})
          coro
          (fiber/setenv (table/setproto @{:out outbuf :err outbuf :parser p} e))
          resume))
      (print "closing client " name))))

(defn client
  "Connect to a repl server. The default host is \"127.0.0.1\" and the default port
  is \"9365\"."
  [&opt host port name]
  (default host default-host)
  (default port default-port)
  (default name (string "[" host ":" port "]"))
  (with [stream (net/connect host port)]
    (def recv (make-recv stream))
    (def send (make-send stream))
    (send name)
    (while true
      (def p (recv))
      (if (not p) (break))
      (def line (getline p @"" root-env))
      (if (empty? line) (break))
      (send (if (keyword? line) (string "\xFE" line) line))
      (prin (or (recv) "")))))

