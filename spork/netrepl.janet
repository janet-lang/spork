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

#  REPL Protocol
#
# 1. server <- {user specified name of client (will be shown in repl)} <- client
# 2. server -> {repl prompt (no newline)} -> client
# 3. server <- {one chunk of input} <- client
# 4. server -> {(dyn :out) and (dyn :err) (empty at first)} -> client
# 5. go back to 2.

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
  per connection. If env is a function, that funciton will be invoked with
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
      (defer (:close stream)
        (def recv (make-recv stream))
        (def send (make-send stream))
        (set name (or (recv) (break)))
        (print "client " name " connected")
        (def e (coerce-to-env env name stream))
        (put e :out outbuf)
        (put e :err outbuf)
        (var is-first true)
        (defn getline-async
          [prmpt buf]
          (if is-first
            (set is-first false)
            (do
              (send outbuf)
              (buffer/clear outbuf)))
          (send prmpt)
          (if-let [msg (recv)] (buffer/push-string buf msg)))
        (defn chunk
          [buf p]
          (def delim (parser/state p :delimiters))
          (def lno ((parser/where p) 0))
          (getline-async (string name ":" lno ":" delim  " ") buf))
        (defn pp-wrap [x] (pp x) x)
        (->
          (repl chunk (make-onsignal getline-async e e 1) e)
          coro
          (fiber/setenv e)
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
      (def line (getline p @"" root-env)) # use root-env as approximation
      (if (empty? line) (break))
      (send line)
      (prin (recv)))))

