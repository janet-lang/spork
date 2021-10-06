(import ../spork/temple :as temple)
(import ../spork/test)

(temple/add-loader)

(test/start-suite 8)

(defn check-template
  [template args expected]
  (def buf @"")
  (with-dyns [:out buf]
    (template args))
  (def sbuf (string/trim (string buf)))
  (test/assert (= sbuf expected) (string "Render of " template)))

(import ./templates/hi :as hi)
(import ./templates/hop :as hop)

(check-template hi/render-dict {:a 1 :b 2}
                ```
                <html>
                  6
                </html>
                ```)

(check-template hop/render-dict {:a 1 :b 2}
                ```
                <html>
                  6
                </html>
                ```)

(def str-template "<html>hello {{ (args :arg) }}</html>")
(def render (temple/compile str-template))
(def out (render :arg "world"))

(test/assert (buffer? out) "Rendered temple string produces a buffer")

(def expected "<html>hello world</html>")
(test/assert (= expected (string out)) "Rendered temple string produces \"hello world\"")

(test/assert (deep= ((temple/compile `{. (args :a) " * 2 = " (args :b) .}`) :a 1 :b 2)
                    @"1 * 2 = 2")
             "concat chunk")

(test/end-suite)
