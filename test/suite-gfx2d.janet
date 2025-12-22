(use ../spork/test)
(import ../spork/path)
(use spork/gfx2d)

(start-suite)

(assert true)

##
## Please keep gold images small on disk to avoid large repository sizes (no larger than 256x256).
## Make liberal use of `resize` to shrink images before calling `check-image` on them.
##

(defn- save
  "Save image to disk"
  [img name]
  (print "Saving image " name)
  (case (path/ext name)
    ".png" (save-png name img)
    ".jpeg" (save-jpg name img 100) # Testing against jpeg is risky - lossy format.
    ".jpg" (save-jpg name img 100)
    ".bmp" (save-bmp name img)
    ".tga" (save-tga name img)
    (errorf "unknown image format of %s" name)))

(defn check-image
  "Either save image to a directory or compare against the existing image"
  [img file-name]
  (def fullpath (path/join "test" "gold" file-name))
  (def tmppath (path/join "tmp" file-name))
  (os/mkdir "tmp")
  (save img tmppath)
  (if (or (os/getenv "GOLD")
          (os/getenv (string "GOLD_" (first (string/split "." file-name))))
          (not (os/stat fullpath :mode)))
    (save img fullpath)
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

(defn test-simple-text-4
  []
  (def [w h] (measure-simple-text "Hello, world!" :olive))
  (def canvas (blank w h 3))
  (draw-simple-text canvas 0 0 1 1 "Hello, world!" white :olive)
  (check-image canvas "hello_text_center.png"))

(test-simple-text-4)

(defn test-path-fill-1
  []
  (def canvas (blank 65 65 4))
  (def points
    [0 32
    32 0
    64 32
    32 64])
  (fill-path canvas points cyan)
  (check-image canvas "path_fill_1.png"))

(test-path-fill-1)

(defn test-star
  []
  (def width 1024)
  (def height 1024)
  (def img (blank width height 4))
  (def num-points 50)
  (each [r color] [[400 yellow] [300 green] [200 blue]]
    (def points @[])
    (for i 0 (* 2 num-points)
      (def theta (/ (* i math/pi) num-points))
      (def radius (if (odd? i) r (/ (* r 153) 400)))
      (def x (+ (/ width 2) (* radius (math/cos theta))))
      (def y (+ (/ height 2) (* radius (math/sin theta))))
      (array/concat points [(math/round x) (math/round y)]))
    (fill-path img points color))
  (def smaller (resize img 128 128))
  (check-image smaller "star.png"))

(test-star)

(defn test-concave-fill-1
  []
  (def canvas (blank 65 65 4))
  (def points
    [0 32
    32 0
    64 32
    50 64
    # concave part
    40 63
    20 63
    #
    14 64])
  (fill-path canvas points cyan)
  (check-image canvas "concave_fill_1.png"))

(test-concave-fill-1)

(defn test-stroke-bezier
  []
  (def width 256)
  (def height 256)
  (def canvas (blank width height 3))
  (def control-points [10 10
                       10 (- height 10)
                       (- width 10) (- height 10)
                       (- width 10) 10])
  (def points (bezier-path control-points))
  (stroke-path canvas points green false)
  (check-image canvas "bezier1.png"))

(test-stroke-bezier)

(defn test-fill-bezier
  []
  (def width 256)
  (def height 256)
  (def canvas (blank width height 3))
  (def control-points [10 10 10 (- height 10) (- width 10) (- height 10) (- width 10) 10])
  (def points (map math/round (bezier-path control-points 0.01)))
  (fill-path canvas points yellow)
  (check-image canvas "bezier2.png"))

(test-fill-bezier)

(defn test-fill-bezier-3
  "Test self intersecting path"
  []
  (def width 256)
  (def height 256)
  (def canvas (blank width height 3))
  (def control-points [10 10
                       (+ width 110) 10
                       -90 (- height 10)
                       (- width 10) (- height 10)
                       ])
  (def points (map math/round (bezier-path control-points 0.001)))
  (fill-path canvas points yellow)
  (stroke-path canvas points green)
  (check-image canvas "bezier3.png"))

(test-fill-bezier-3)

(defn test-fill-donut
  "Test a donut path"
  []
  (def width 256)
  (def height 256)
  (def canvas (blank width height 3))
  (def num-points 50)
  (def points @[])
  (each [radius switch] [[50 1] [60 -1]]
    (for i 0 (* 2 (+ num-points 0.5))
      (def theta (/ (* i math/pi) num-points))
      (def x (+ (/ width 2) (* radius (math/cos theta))))
      (def y (+ (/ height 2) (* radius (math/sin (* switch theta)))))
      (array/concat points [x y])))
  (fill-path canvas points blue)
  (check-image canvas "donut.png"))

(test-fill-donut)

(defn test-bumpy-chart
  "Test bumpy chart for fill path"
  []
  (def img (blank 1024 1024 4))
  (rect img 0 0 10000 10000 black)
  (math/seedrandom 0)
  (def xs (range 1000))
  (def ys (seq [x :in xs] (* 100 (+ (math/log (inc x)) (math/random)))))
  (def xformed-xs (map (partial + 12) xs))
  (def xformed-ys (map (partial - 1012) ys))
  (def path (map math/round (mapcat tuple xformed-xs xformed-ys)))
  (fill-path img [;path 1012 1000 12 1000] blue)
  (check-image (resize img 256 256) "bumpy-chart.png"))

(test-bumpy-chart)

(end-suite)
