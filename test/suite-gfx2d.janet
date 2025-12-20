(use ../spork/test)
(import ../spork/path)
(use spork/gfx2d)

(start-suite)

(assert true)

##
## Please keep gold images small on disk to avoid large repository sizes
##

(defn check-image
  "Either save image to a directory or compare against the existing image"
  [img file-name]
  (def fullpath (path/join "test" "gold" file-name))
  (print (string "GOLD_" (first (string/split "." file-name))))
  (if (or (os/getenv "GOLD") (os/getenv (string "GOLD_" (first (string/split "." file-name))))
          (not (os/stat file-name :mode)))
    (case (path/ext file-name)
      ".png" (save-png fullpath img)
      ".jpeg" (save-jpg fullpath img 100) # Testing against jpeg is risky - lossy format.
      ".jpg" (save-jpg fullpath img 100)
      ".bmp" (save-bmp fullpath img)
      ".tga" (save-tga fullpath img)
      (errorf "unknown image format of %s" file-name))
    (do
      (def reference (load fullpath))
      (assert (deep= reference img) (string "reference not identical to test image " file-name)))))

(defn test-image-1
  []
  (def img (blank 128 128 3))
  (rect img 16 16 112 112 red)
  (rect img 32 32 96 96 blue)
  (circle img 64 64 30.5 yellow)
  (check-image img "target1.png")
  (check-image img "target1.bmp")
  (check-image img "target1.tga"))

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
  (check-image smaller "small_stamp1.bmp"))

(test-stamp)

(defn test-blank
  []
  (def img (blank 154 113 3))
  (check-image img "blank.png"))

(test-blank)

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
  (assert (deep= cop img))
  (def empty1 (diff cop img))
  (def empty2 (diff img img))
  (check-image empty1 "empty.png")
  (check-image empty2 "empty2.tga"))

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

(defn test-simple-text
  []
  (def canvas (blank 128 16 3))
  (draw-simple-text canvas 2 2 1 1 "Hello, world!" white)
  (check-image canvas "hello_text.png"))

(test-simple-text)

(defn test-simple-text-2
  []
  (def canvas (blank 128 16 3))
  (draw-simple-text canvas 2 2 1 1 "Hello, world!" white :tall)
  (check-image canvas "hello_text_tall.png"))

(test-simple-text-2)

(defn test-simple-text-3
  []
  (def canvas (blank 128 16 3))
  (draw-simple-text canvas 2 2 1 1 "Hello, world!" white :olive)
  (check-image canvas "hello_text_olive.png"))

(test-simple-text-3)

(defn test-path-fill-1
  []
  (def canvas (blank 65 65 4))
  (def points
    [0 32
    32 0
    64 32
    32 64])
  (fill-path-2 canvas points cyan)
  (check-image canvas "path_fill_1.png"))

(test-path-fill-1)

(end-suite)
