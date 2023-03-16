(import spork/cjanet :as c)

(c/include <janet.h>)
(c/include <stdio.h>)

(c/function my_fn
  static
  "Sum 3 numbers together (just to test writing c functions)"
  [x:double y:int z:int] -> double
  (return (+ x y z)))

(c/function
  rpn_calculator
  "Simple calculator function"
  [(command (const (* char)))] -> double
  (def bob:int 25)
  (def (stack (array double 1024)))
  (return 10.0))
        
(c/cfunction my-function
  "Does a thing."
  [x:double y:double &opt z:double=10.4]
  (return (janet_wrap_number (+ x y z))))

(c/module-entry "my-module")
