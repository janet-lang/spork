(import ../spork/temple :as temple)
(import spork/test)

(temple/add-loader)

(test/start-suite 8)

(defn check-template
  [template args expected]
  (def buf @"")
  (with-dyns [:out buf]
    (template args)
    (def sbuf (string/trim (string buf)))
    (test/assert (= sbuf expected) (string "Render of " template))))

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

(test/end-suite)
