(import ../spork/temple :as temple)
(import ../spork/test)

(temple/add-loader)

(test/start-suite 8)

(defn- remove-r [x] (string/replace-all "\r" "" x))

(defn check-template
  [template args expected]
  (def expected (string/trim (remove-r expected)))
  (def buf @"")
  (with-dyns [:out buf]
    (template args))
  (def sbuf (string/trim (remove-r (string buf))))
  (test/assert (= sbuf expected) (string/format "template %v - expected %v, got %v" template expected sbuf)))

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

(def ctc
  `{$ (import /spork/fmt) $}{{ (fmt/format (string "(def c    " (args :a) " )")) }}`)

(test/assert (deep= ((temple/compile ctc) :a "a > b")
                    @"(def c a &gt; b)\n")
             "compile time chunk")

(test/assert (deep= ((temple/compile `{{ (args :a) }}`) :a "a > b")
                    @"a &gt; b")
             "sub chunk")

(test/assert (deep= ((temple/compile `{% (print (args :a)) %}`) :a "a > b")
                    @"a > b\n")
             "code chunk")

(test/assert (deep= ((temple/compile `{- (args :a) -}`) :a "a > b")
                    @"a > b")
             "raw chunk")

(test/end-suite)
