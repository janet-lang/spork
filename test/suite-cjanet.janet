(use ../spork/test)
(use ../spork/cjanet)

(start-suite)

(assert true) # smoke

###
### JIT funcitonality
###

(begin-jit
  :build-type :release
  :cflags ["-Werror"])

(typedef Bill
  (struct
    a double
    b float
    c char
    d-dash (* char)))

(typedef Bob
  (named-struct Bob
    x int
    y int
    z int))

(cfunction
  add-two
  "Add 2 integers together"
  [x:int y:int] -> int
  (def bob:Bob (struct x x y y z 1))
  (return (+ x y)))

(cfunction
  add-three
  "Add 3 integers together"
  [x:int y:int z:int] -> int
  (return (+ x (add-two y z))))

(end-jit)

(assert (= 6 (add-three 1 2 3)) "add-three")
(assert (= 20 (add-two 15 5)) "add-two")

(end-suite)
