(import spork/cjanet :as c)

(c/include <janet.h>)

(defn- make-binop
  [op]
  ~(do
     (if (< s 2) (janet_panic "stack empty"))
     (-- s)
     (set (aref stack (- s 1)) (,(symbol op) (aref stack s) (aref stack (- s 1))))))

(c/emit-cfunction
  'rpn :static
  "Simple RPN calculator"
  ;'[
     [command:cstring] -> double
     (def (stack (array double 1024)))
     (def s:int 0)
     (def (c (* char)) command)
     (while (deref c)
       (def (oldc (* char)) c)
       (def d:double (strtod c (addr c)))
       (def x:int (deref c))
       (cond
         (not= oldc c) (do (set (aref stack s) d) (++ s))
         (== x ,(chr `+`)) ,(make-binop :+)
         (== x ,(chr `-`)) ,(make-binop :-)
         (== x ,(chr `*`)) ,(make-binop :*)
         (== x ,(chr `/`)) ,(make-binop :/))
       (if (== oldc c) (++ c)))
     (return (aref stack (- s 1)))])

(c/module-entry "my-module")
