(use ../spork/test)
(import ../spork/path)
(use spork/gfx2d)

(start-suite)

(assert true)

(defn check-image
  "Either save image to a directory or compare against the existing image"
  [img file-name]
  (if (os/getenv "GOLD")
    (save-png (path/join "test" "gold" file-name) img)
    (do
      (def reference (load (path/join "test" "gold" file-name)))
      (assert (deep= reference img) "reference not identical to test image"))))

(defn test-image-1
  []
  (def img (blank 128 128 3))
  (rect img 16 16 112 112 red)
  (rect img 32 32 96 96 blue)
  (circle img 64 64 30.5 yellow)
  (check-image img "target1.png"))

(test-image-1)

(defn test-stamp
  []
  (def img (blank 128 128 3))
  (rect img 16 16 112 112 red)
  (rect img 32 32 96 96 blue)
  (circle img 64 64 30.5 yellow)
  (def dest (blank 1024 1024 3))
  # Don't crash for oob
  (stamp dest img 512 -512)
  (stamp dest img -512 -512)
  (stamp dest img 512 512)
  (stamp dest img 512 1512)
  (stamp dest img 1512 512)
  (stamp dest img 1512 1512)
  (loop [x :range [-64 1024 128]
         y :range [-64 1024 128]]
    (stamp dest img x y))
  (check-image dest "stamp1.png")
  (def smaller (resize dest 128 128))
  (check-image smaller "small_stamp1.png"))

(test-stamp)

(defn test-copy
  []
  (defn copy [img]
    (def [buf w h c] img)
    [(buffer/slice buf) w h c])
  (def img (blank 154 113 3))
  (rect img 16 16 112 112 red)
  (circle img 16 16 1000 cyan) # oob circle
  (rect img 32 32 96 96 blue)
  (def cop (copy img))
  (def empty (diff cop img))
  (def empty2 (diff img img))
  (check-image empty "empty.png")
  (check-image empty2 "empty2.png"))

(test-copy)

(defn test-lines
  []
  (def dest (blank 1024 1024 3))
  (rect dest 0 0 1024 1024 blue)
  (loop [theta :range [0 (* 2 math/pi) (/ math/pi 24)]]
    (def sin (math/sin theta))
    (def cos (math/cos theta))
    (line dest
          (math/round (+ 512 (* cos 10))) (math/round (+ 512 (* sin 10)))
          (math/round (+ 512 (* cos 500))) (math/round (+ 512 (* sin 500)))
          magenta))
  (check-image dest "line_circle.png"))

(test-lines)

(defn test-triangle
  []
  (def tri (blank 128 128 3))
  (triangle tri 64 0 0 127 127 127 red)
  (check-image tri "triange.png"))

(test-triangle)

(defn test-triangle2
  []
  (def tri (blank 128 128 3))
  (triangle2 tri 64 0 0 127 127 127 red)
  (check-image tri "triange2.png"))

(test-triangle2)

(end-suite)
