###
### HTML generation using plain data.
###

(def- escape-peg
  (peg/compile
    ~(% (any (+ (* "&" (constant "&amp;"))
                (* "\"" (constant "&quot;"))
                (* "<" (constant "&lt;"))
                (* ">" (constant "&gt;"))
                (* "'" (constant "&#39;"))
                '1)))))
(defn escape
  "Escape characters in a string for HTML"
  [x]
  (in (peg/match escape-peg (string x)) 0))

(var- render nil)

(defn- render-each
  [to arr]
  (each el arr
    (render to el)))

(defn- render-attrs
  [to attrs]
  (eachk k attrs
    (buffer/push to " " k `="` (escape (get attrs k)) `"`)))

(defn- render-normal-tag
  [to tag attrs children]
  (buffer/push to "<" tag)
  (render-attrs to attrs)
  (buffer/push to ">")
  (render-each to children)
  (buffer/push to "</" tag ">"))

(defn- render-self-closed-tag
  [to tag attrs]
  (buffer/push to "<" tag)
  (render-attrs to attrs)
  (buffer/push to "/>"))

(def- self-close-tags
  {:area true
   :base true
   :br true
   :col true
   :embed true
   :hr true
   :img true
   :input true
   :link true
   :meta true
   :param true
   :source true
   :track true
   :wbr true
   :command true
   :keygen true
   :menuitem true})

(defn- render-tag
  [to tag attrs children]
  (if (get self-close-tags tag)
    (render-self-closed-tag to tag attrs)
    (render-normal-tag to tag attrs children)))

(defn- render-tuple
  [to tup]
  (def [tag maybe-attrs] tup)
  (assert tag "expected tag")
  (if (dictionary? maybe-attrs)
    (render-tag to tag maybe-attrs (drop 2 tup))
    (render-tag to tag {} (drop 1 tup))))

(defn- render-bytes
  [to bytes]
  (buffer/push to (escape bytes)))

(defn- render-function
  [to f]
  (f to))

(def- type-renders
  {:tuple render-tuple
   :array render-each
   :string render-bytes
   :buffer render-bytes
   :fiber render-each
   :number render-bytes
   :boolean render-bytes
   :nil (fn [&])
   :function render-function})

(defn- render1
  [to x]
  (def handler (get type-renders (type x)))
  (if handler
    (handler to x)
    (errorf "cannot render %V" x)))

(set render render1)

###
### Public API
###

(defn raw
  "Get an object that can be used to splice in HTML literals. 
  `text` is not escaped in the output string."
  [text]
  (fn [buf] (buffer/push buf text)))

(def doctype-html
  "The html5 doctype header"
  (raw "<!DOCTYPE html>"))

(defn html
  "Render HTML from standard data structures. Fills the provided optional 
  buffer, or new one if it is not provided, with the html bytes."
  [data &opt buf]
  (default buf @"")
  (render buf data)
  buf)
