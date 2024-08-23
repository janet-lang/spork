(use ../spork/test)
(import ../spork/htmlgen :as htmlgen)

(start-suite)

(var check-count 0)
(defn check-render
  [input expected-output]
  (def buf (htmlgen/html input))
  (def msg (string "render check " (++ check-count)))
  (assert (= expected-output (string buf)) msg))

(check-render "abc" "abc")
(check-render "abc&<>\"'" "abc&amp;&lt;&gt;&quot;&#39;")
(check-render "a b c" "a b c")
(check-render [:div "abc"] "<div>abc</div>")
(check-render [:div] "<div></div>")
(check-render [:div 123] "<div>123</div>")
(check-render [:div {:class "big green"} "thing"] "<div class=\"big green\">thing</div>")

(end-suite)
