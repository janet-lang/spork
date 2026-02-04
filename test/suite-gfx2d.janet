(use ../spork/test)
(import ../spork/path)

(use spork/gfx2d)

# Test charting as well
(import spork/charts :as charts)

(start-suite)

(assert true)

##
## Please keep gold images small on disk to avoid large repository sizes (usually no larger than 256x256).
## Make liberal use of `resize` to shrink images before calling `check-image` on them.
##

(defn- freeze-image
  [img]
  (def {:width w :height h :channels c :data d :stride s} (unpack img))
  [w h c s (ffi/pointer-buffer d (* c w h) (* c w h))])

(defn check-image
  "Either save image to a directory or compare against the existing image"
  [img file-name]
  (def fullpath (path/join "test" "gold" file-name))
  (def tmppath (path/join "tmp" file-name))
  (os/mkdir "tmp")
  (save tmppath img)
  (when (or (os/getenv "GOLD")
          (os/getenv (string "GOLD_" (first (string/split "." file-name))))
          (not (os/stat fullpath :mode)))
    (print "Saving gold image " fullpath)
    (save fullpath img)
    (break))
  (def reference (load fullpath))
  (assert (deep= (freeze-image reference) (freeze-image img)) (string "reference not identical to test image " file-name)))

(defn test-image-1
  []
  (def img (blank 128 128 3))
  (fill-rect img 16 16 96 96 red)
  (fill-rect img 32 32 64 64 blue)
  (circle img 64 64 30.5 yellow)
  (check-image img "target1.png")
  (check-image img "target1.bmp")
  (check-image img "target1.tga"))

(test-image-1)

(defn test-stamp
  []
  (def img (blank 128 128 3))
  (fill-rect img 16 16 96 96 red)
  (fill-rect img 32 32 64 64 blue)
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
  (def img (blank 154 113 3))
  (fill-rect img 16 16 96 96 red)
  (circle img 16 16 1000 cyan) # oob circle
  (fill-rect img 32 32 64 64 blue)
  (def cop (copy img))
  (assert (deep= (freeze-image cop) (freeze-image img)))
  (def empty1 (diff cop img))
  (def empty2 (diff img img))
  (check-image empty1 "empty.png")
  (check-image empty2 "empty2.tga"))

(test-copy)

(defn test-simple-text
  []
  (def canvas (blank 128 16 3))
  (draw-simple-text canvas 2 2 "Hello, world!" white)
  (check-image canvas "hello_text.png"))

(test-simple-text)

(defn test-simple-text-2
  []
  (def canvas (blank 128 16 3))
  (draw-simple-text canvas 2 2 "Hello, world!" white :tall)
  (check-image canvas "hello_text_tall.png"))

(test-simple-text-2)

(defn test-simple-text-3
  []
  (def canvas (blank 128 16 3))
  (draw-simple-text canvas 2 2 "Hello, world!" white :olive)
  (check-image canvas "hello_text_olive.png"))

(test-simple-text-3)

(defn test-simple-text-4
  []
  (def [w h] (measure-simple-text "Hello, world!" :olive))
  (def canvas (blank w h 3))
  (draw-simple-text canvas 0 0 "Hello, world!" white :olive)
  (check-image canvas "hello_text_center.png"))

(test-simple-text-4)

(defn test-simple-text-cp437
  []
  (def text "£√÷a⌠⌡δ☻bc123")
  (def [w h] (measure-simple-text text :olive 2 2))
  (def canvas (blank w h 3))
  (draw-simple-text canvas 0 0 text (rgb 0.7 0.7 0.7) :olive 2 2)
  (check-image canvas "cp437.png"))

(test-simple-text-cp437)

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
  # (check-image img "bigstar.png")
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
  (stroke-path canvas points green 4)
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
  (stroke-path canvas points green 4.5 true)
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
  #(stroke-path canvas points magenta 4)
  (check-image canvas "donut.png"))

(test-fill-donut)

(defn test-bumpy-chart
  "Test bumpy chart for fill path"
  []
  (def img (blank 1024 1024 4))
  (fill-rect img 0 0 10000 10000 black)
  (math/seedrandom 0)
  (def xs (range 1000))
  (def ys (seq [x :in xs] (* 100 (+ (math/log (inc x)) (math/random)))))
  (def xformed-xs (map (partial + 12) xs))
  (def xformed-ys (map (partial - 1012) ys))
  (def path (map math/round (mapcat tuple xformed-xs xformed-ys)))
  (def path2 [;path 1012 1000 12 1000])
  (fill-path img path2 blue)
  #(check-image img "big-bumpy-chart.png")
  (check-image (resize img 256 256) "bumpy-chart.png")
  (fill-rect img 0 0 10000 10000 black)
  (def path2 (mapcat identity (reverse (partition 2 path2))))
  (fill-path img path2 green)
  (check-image (resize img 256 256) "bumpy-chart-2.png"))

(test-bumpy-chart)

(defn test-blend
  "Test default blending"
  []
  (def img (blank 128 128 4))
  (fill-rect img 0 0 128 128 black)
  (loop [col :range [0 128 8]]
    (fill-rect img col 0 8 128
               (if (even? (div col 8))
                 (rgb 0.9 0.9 0.8)
                 (rgb 0.1 0.1 0.1))))
  (def sonic (blank 64 64 4))
  (each [radius color] [[10 green] [20 yellow] [30 blue]]
    (plot-ring sonic 32 32 radius color))
  (stamp-blend img sonic :over 32 32)
  (check-image img "blend-ring.png"))

(test-blend)

# Charting test
(defn test-temperature-chart
  "Test the chart module"
  []
  (def npoints 100)
  (def rng (math/rng 10))
  (def data
    {:timestamp (map |(/ $ 10) (range npoints))
     :temperature-1 (seq [i :range [0 npoints]] (+ (math/log (+ i 1)) (* 0.3 (math/rng-uniform rng))))
     :temperature-2 (seq [i :range [0 npoints]] (+ (* 0.94 (math/log (+ i 1))) (* 0.2 (math/rng-uniform rng))))
     :temperature-3 (seq [i :range [0 npoints]] (+ (* 0.79 (math/log (+ i 1))) (* 0.4 (math/rng-uniform rng))))
     :temperature-4 (seq [i :range [0 npoints]] (+ (* 0.45 (math/log (+ i 8))) (* 0.4 (math/rng-uniform rng))))
     })
  (def columns [:temperature-1 :temperature-2 :temperature-3 :temperature-4])
  (def img
    (charts/line-chart
      :title "Data over Time"
      :width 512
      :height 512
      :data data
      :x-column :timestamp
      :padding 10
      :font :olive
      :grid :solid
      :circle-points true
      :legend :top
      :legend-map (tabseq [c :in columns] c (string/replace "temperature-" "T" c))
      :y-column columns))

  (check-image img "complex_chart.png"))

(test-temperature-chart)

(defn test-bar-chart
  []
  (with-dyns []
    (charts/dark-mode)

    # Get axes
    (def canvas (blank 1920 1080))
    (fill-rect canvas 0 0 2000 2000 black)
    (def [view convert]
      (charts/draw-axes
        canvas
        :padding 4
        :format-y |(string/format "$%.2f" $)
        :x-label "Units"
        :y-label "Dollars"
        :y-min 0
        :x-ticks (range 0 11)
        :x-labels-vertical true
        :x-min -0.5
        :x-max 10.5
        :y-max 100))

    # Bar chart
    (charts/plot-line-graph
      :canvas view
      :to-pixel-space convert
      :x-column :x
      :y-column :y
      :data {:x (range 0 11) :y (seq [x :range [0 11]] (+ 50 (* 40 (math/sin (* 1 x)))))}
      :line-style :bar)

    # Lets add a legend in the top right corner
    (def legend-args [:labels ["Thing 1" "Thing 2"] :frame true :padding 4])
    (def [lw lh] (charts/draw-legend nil ;legend-args))
    (def {:width vw :height vh} (unpack view))
    (def legend-view (viewport view (- vw lw 10) 10 lw lh true))
    (charts/draw-legend legend-view ;legend-args)

    # Check final image
    # (check-image canvas "big-bar-chart.png")
    (check-image (resize canvas 192 108) "bar-chart.png")))

(test-bar-chart)

(end-suite)
