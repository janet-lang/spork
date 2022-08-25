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

(def s (httpf/server))
(httpf/listen s)
