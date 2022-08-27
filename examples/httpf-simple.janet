(import spork/httpf)

(defn hello
  "Simple hello world route."
  {:path "/"}
  [&]
  "Hello, world!")

(defn what-time-is-it
  "What time is it?"
  {:path "/what-time"}
  [&]
  (ev/sleep 0.1)
  @[[:h1 "Current Unix Time"]
    [:p (string (os/time))]])

(defn post-double
  "Post a number and get it back doubled. (also can use GET with ?data=10 for easy testing)."
  {:path "/double"
   :schema :number}
  [req data]
  (* data 2))

(-> (httpf/server)
    httpf/add-bindings-as-routes
    httpf/listen)
