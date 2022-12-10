###
### Render an MDZ document to HTML.
### Run from the repository root and run `janet examples/mdzrender.janet`
### to generate a file temp.html in the repository root.
###

(import spork/htmlgen)
(import spork/mdz)

(def mu (mdz/markup (slurp "examples/example.mdz")))
(def dom (get mu :markup-dom))
(def html (htmlgen/html dom))
(spit "temp.html" html)

