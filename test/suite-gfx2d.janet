(use ../spork/test)
(use spork/gfx2d)

(start-suite)

(assert true)

(os/mkdir "tmp")

(defn test-image-1
  []
  (def img (blank 128 128 3))
  (rect img 16 16 112 112 red)
  (rect img 32 32 96 96 blue)
  (circle img 64 64 30.5 yellow)
  (assert (= yellow (pixel img 64 64)) "check pixel 1"))
(test-image-1)

(end-suite)
