(use spork/test)
(import spork/math :as smath)
(import spork/path)

(use spork/gfx2d)

# Test charting as well
(import spork/charts :as charts)

# Don't make hash dependent tests - not easily portable across versions
(setdyn charts/*color-seed* (os/cryptorand 16))

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
  [img file-name &opt diff-threshold]
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
  (if diff-threshold
    # Fuzzy-compare
    (let [f-img (freeze-image img)
          f-ref (freeze-image reference)]
      # Compare w, h, channels, and stride
      (assert (deep= (take 4 f-img) (take 4 f-ref)) (string "reference dimensions not identical to test image dimensions " file-name))
      # For pointer buffer, allow for some differences given a threshold
      # TODO - create C function(s) for image statistics
      (def f-img-buf (get f-img 4))
      (def f-ref-buf (get f-ref 4))
      (def blen (length f-img-buf))
      (var total-diff 0)
      (var num-pixels-diff 0)
      (for i 0 blen
        # Compare each byte and given absdiff. No sRGB considerations.
        (def absdiff (math/abs (- (in f-img-buf i) (in f-ref-buf i))))
        (if (> absdiff 0) (++ num-pixels-diff))
        (+= total-diff absdiff))
      (def diff (/ total-diff blen))
      (assert (< diff diff-threshold) (string/format "difference between reference and test image (%.3f) is beyond threshold (%.3f) for %s - %d pixels different" diff diff-threshold file-name num-pixels-diff)))
    # No fuzzy-compare
    (assert (deep= (freeze-image reference) (freeze-image img)) (string "reference not identical to test image " file-name))))

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

(defn test-stroke-bezier-stipple
  []
  (def width 256)
  (def height 256)
  (def canvas (blank width height 3))
  (def control-points [10 10
                       10 (- height 10)
                       (- width 10) (- height 10)
                       (- width 10) 10])
  (def points (bezier-path control-points 0.04))
  (stroke-path canvas [0 0 (/ width 2) (/ height 2) width height] red 4 false 20 10)
  (stroke-path canvas points green 4 false 20 10)
  (check-image canvas "bezier-stipple.png"))

(test-stroke-bezier-stipple)

(defn test-stroke-bezier-stipple-2
  []
  (def width 256)
  (def height 256)
  (def canvas (blank width height 3))
  (def control-points [10 10
                       10 (- height 10)
                       (- width 10) (- height 10)
                       (- width 10) 10])
  (def points (bezier-path control-points 0.004))
  (stroke-path canvas [0 0 (/ width 2) (/ height 2) width height] red 4 false 10 5)
  (stroke-path canvas points green 4 false 10 5)
  (check-image canvas "bezier-stipple-2.png"))

(test-stroke-bezier-stipple-2)

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
                       (- width 10) (- height 10)])
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
  # Order of calculation must be deterministic between Janet versions! Do NOT inline this into the struct in `data`.
  (def t1 (seq [i :range [0 npoints]] (+ (math/log (+ i 1)) (* 0.3 (math/rng-uniform rng)))))
  (def t2 (seq [i :range [0 npoints]] (+ (* 0.94 (math/log (+ i 1))) (* 0.2 (math/rng-uniform rng)))))
  (def t3 (seq [i :range [0 npoints]] (+ (* 0.79 (math/log (+ i 1))) (* 0.4 (math/rng-uniform rng)))))
  (def t4 (seq [i :range [0 npoints]] (+ (* 0.45 (math/log (+ i 8))) (* 0.4 (math/rng-uniform rng)))))
  (def data
    {:timestamp (map |(/ $ 10) (range npoints))
     :temperature-1 t1
     :temperature-2 t2
     :temperature-3 t3
     :temperature-4 t4})
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
      :circle-points :oplot
      :color-map {:temperature-1 blue :temperature-2 green :temperature-3 yellow :temperature-4 cyan}
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
        :canvas canvas
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
      :color-map {:y blue}
      :line-style :bar)

    # Lets add a legend in the top right corner
    (def legend-args [:labels ["Thing 1" "Thing 2"] :frame true :padding 4 :color-map {"Thing 1" blue "Thing 2" green}])
    (def [lw lh] (charts/draw-legend nil ;legend-args))
    (def {:width vw :height vh} (unpack view))
    (def legend-view (viewport view (- vw lw 10) 10 lw lh true))
    (charts/draw-legend legend-view ;legend-args)

    # Check final image
    #(check-image canvas "big-bar-chart.png")
    (check-image (resize canvas 192 108) "bar-chart.png")))

(test-bar-chart)

(defn test-tabs-newlines-simple
  []
  (def canvas (blank 128 128 3))
  (draw-simple-text canvas 2 2 "Hello, world!\nabc\n\t123" white)
  (check-image canvas "tabs_newlines_text.png"))

(test-tabs-newlines-simple)

(defn test-tabs-newlines-ttf
  []
  (def canvas (blank 256 256 3))
  (def font (load-font "examples/fonts/Roboto-Regular.ttf" 18))
  (draw-text canvas font 2 2 "Hello, world!\nabc\n\t123" white)
  # TODO - decrease the diff threshold
  (check-image canvas "tabs_newlines_ttf_text.png" 2.0))

(test-tabs-newlines-ttf)

(defn test-text-pinwheel
  []
  (def canvas (blank 200 200 3))
  (each o [0 1 2 3]
    (draw-simple-text canvas 100 100 "Pinwheel\npretty\n\tcool" yellow :default 1 1 o))
  (check-image canvas "text_pinwheel.png"))

(test-text-pinwheel)

(defn test-heat-map-1
  [mapping]
  (def cmap (get charts/color-maps mapping))
  (defn distfrom [px py] (fn [x y] (let [dx (- px x) dy (- py y)] (math/sqrt (+ (* dx dx) (* dy dy))))))
  (def d1 (distfrom 10 10))
  (def d2 (distfrom 20 18))
  (def d3 (distfrom 37 8))
  (def chart
    (charts/heat-map-chart
      :width (* 0.2 1920) :height (* 0.2 1080)
      :num-columns (* 1 48) :num-rows (* 1 27)
      :font (load-font "examples/fonts/Roboto-Regular.ttf" 12)
      :title (string "Heat Map Distance Test " (string/ascii-upper mapping))
      #:cell-text-color 0xFFFFFFFF
      #:cell-text-fn (fn [x y] (string/format "%d,%d" x y))
      :color-fn (fn [x y]
                  (def t (min (* 0.1 (d1 x y)) (* 0.2 (d2 x y)) (* 0.03 (d3 x y))))
                  (cmap (+ (* 0.01 (math/random)) t)))))
  (check-image chart (string "heat_map_" mapping ".png")))

(test-heat-map-1 :viridis)
(test-heat-map-1 :turbo)
(test-heat-map-1 :grayscale)
(test-heat-map-1 :magma)
(test-heat-map-1 :bluescale)

(defn test-multi-chart
  []
  (def big-canvas (blank 512 512 4))
  (def nw-canvas (viewport big-canvas 0 0 256 256 true))
  (def ne-canvas (viewport big-canvas 256 0 256 256 true))
  (def sw-canvas (viewport big-canvas 0 256 256 256 true))
  (def se-canvas (viewport big-canvas 256 256 256 256 true))
  (eachp [i sector] [nw-canvas ne-canvas sw-canvas se-canvas]
    (charts/line-chart
      :canvas sector
      :data {:x (range 201)
             :y (seq [x :range [0 201]] (* 5 (math/cos (* x (+ 0.04 (* i 0.02))))))}
      :super-sample 4
      :line-style :stroke
      :color-map blue
      :x-column :x
      :y-column :y))
  (check-image big-canvas "4_chart.png"))

(test-multi-chart)

(defn test-super-minimal-chart
  []
  # By default is too large so we shrink it
  (def c (charts/line-chart :padding 0 :inner-padding 0 :width 200 :height 200
                            :color-map red :line-style :stroke :super-sample 4
                            :data {:x (reverse (range 100)) :y (range 100)}))
  (check-image c "minimal_chart.png"))

(test-super-minimal-chart)

(defn test-horizontal-chart
  [line-style]
  (def c
    (charts/line-chart
      :title (string "H " line-style " Chart (sine waves)")
      :data {:x (range 101)
             :y (seq [x :range [0 100]] (math/cos (* x 0.2)))
             :z (seq [x :range [0 100]] (math/sin (* x 0.2)))}
      :line-style line-style
      :color-map {:y blue :z red}
      :super-sample 4
      :transpose true
      :bar-padding 2
      :width 500
      :height 500))
  (check-image c (string "horizontal_" line-style "_chart.png")))

(test-horizontal-chart :area)
(test-horizontal-chart :bar)
(test-horizontal-chart :stroke)
(test-horizontal-chart :multi-bar)

(defn test-axis-points
  "Make sure that drawing axes properly respects the input coordinates and returns good mapping functions."
  [w h xt yt &opt inner-padding]
  (default inner-padding 8)
  (def c (blank w h 4))
  (fill-rect c 0 0 w h white)
  (def [view to-pix to-metric outer]
    (charts/draw-axes
      :canvas c
      :x-min 0 :x-max (- xt 1) :y-min 0 :y-max (- yt 1)
      :grid :stipple
      :inner-padding inner-padding
      :x-ticks (range xt)
      :y-ticks (range yt)))
  (def {:width vw :height vh} (unpack view))
  (loop [x :range [0 xt]
         y :range [0 yt]]
    (def [px py] (to-pix x y))
    # Check 4 corners are in the right places
    (when (= x 0) (assert (smath/approx-eq px inner-padding)))
    (when (= y 0) (assert (smath/approx-eq py (- vh 1 inner-padding))))
    (when (= x (dec xt)) (assert (smath/approx-eq px (- vw 1 inner-padding))))
    (when (= y (dec yt)) (assert (smath/approx-eq py inner-padding)))
    (def [xx yy] (to-metric px py))
    (assert (smath/approx-eq xx x) "bad axes vector mapping")
    (assert (smath/approx-eq yy y) "bad axes vector mapping"))
  # Put blue rings in the 4 corners for visual inspection
  (each x [0 (- vw 1)]
    (each y [0 (- vh 1)]
      (plot-ring view x y 4 blue)))
  (loop [x :range [0 xt]
         y :range [0 yt]]
    (plot-ring view ;(map math/round (to-pix x y)) 3 red))
  (check-image outer (string "axis_test_" w "x" h "px_" xt "_by_" yt ".png")))

(test-axis-points 200 200 6 6)
(test-axis-points 200 200 3 3)
(test-axis-points 200 200 5 7)
(test-axis-points 200 200 11 27)
(test-axis-points 200 200 2 2)
(test-axis-points 200 200 8 8 0)

(defn test-multi-bar-scatter
  []
  (def nx 12)
  (setdyn :grid-color 0xFF000000)
  (def c
    (charts/line-chart
      :title "Months of the year"
      :data {:x ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
             :y (seq [x :range [0 nx]] (math/cos (* x (/ 10 nx))))
             :yy (seq [x :range [0 nx]] (math/cos (* x (/ 10 nx))))
             :yyy (seq [x :range [0 nx]] (math/cos (* x (/ 10 nx))))
             :yyyy (seq [x :range [0 nx]] (if (not= 20 x) (+ 1.3 (math/sin (* x (/ 10 nx))))))}
      :font (load-font "examples/fonts/Roboto-Regular.ttf" 24)
      :color-map :magma
      :inner-padding 25
      :legend-padding 8
      :line-style :multi-bar
      :circle-points :x
      :point-radius 10
      :stroke-thickness 2
      :bar-padding 100
      :legend :top-right
      :transpose false
      :width (/ 3840 4)
      :height (/ 2160 4)
      :grid :stipple))
  (check-image c "multi-bar-scatter.png"))

(test-multi-bar-scatter)

(defn test-packing-chart
  []
  (def df
    {:x [1 2 3 4 5] :y [1 2 3 4 5]})
  (def c (charts/plot-packing-chart :data df :width 200 :height 200))
  (check-image c "packing-chart-simple.png"))

(test-packing-chart)

(defn test-packing-chart-repeated-data
  []
  (def df
    {:x [1 2 1 2 3] :y [1 2 3 4 5]})
  (def c (charts/plot-packing-chart :data df :width 200 :height 200))
  (check-image c "packing-chart-repeat.png"))

(test-packing-chart-repeated-data)

(defn test-packing-chart-custom-color
  []
  (def df
    {:x [1 2 1 2 3] :y [1 2 3 4 5] :c [0 0.2 0.8 0.2 0]})
  (def c (charts/plot-packing-chart :data df :x-column :x :y-column :y :c-column :c :width 200 :height 200))
  (check-image c "packing-chart-custom-color.png"))

(test-packing-chart-custom-color)

(defn test-packing-chart-nested
  []
  (def p "Big Pig")
  (def c "Little Chicken")
  (def w "Medium Cow")
  (def o "Other")
  (def df2
    {:x (range 1 16)
     :y (range 1 16)
     :group [o w w w o p p p p o o c c c c]})
  (def c (charts/plot-packing-chart :data df2
                                    :color-map :turbo
                                    :font :tall
                                    :x-column :x :y-column :y
                                    :group-column :group :width 400 :height 400))
  (check-image c "packing-chart-nested.png"))

(test-packing-chart-nested)

(end-suite)
