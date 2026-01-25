###
### This module is for generating graphs and charts on the CPU and rendering
### them to bitmaps. While not completely general in styling, charts should be general
### purpose in visualizing different kinds of data.
###
### Data is passed to most charts as a "data-frame", which is a table mapping string column names
### to arrays of data points.
###
### Data frame example:
###
### {:timestamp [1 2 3 4 5 6]
###  :temperature-1 [75.1 75.2 75.4 75.5 75.5 75.4]
###  :temperature-2 [55.1 55.4 55.7 60.0 60.4 60.9]}
###

(import spork/gfx2d :as g)

(defn- int-formatter
  "Round to integer and convert to string"
  [x]
  (string/format "%d" (math/round x)))

(defn- floorn
  "Floor mod n"
  [n x]
  (def x (if (= 0 x) (math/abs x) x)) # no negative 0, messes up rendering!
  (/ (math/floor (* n x)) n))

(defn- roundn
  "Round mod n"
  [n x]
  (def x (if (= 0 x) (math/abs x) x)) # no negative 0, messes up rendering!
  (/ (math/round (* n x)) n))

(defn- color-hash
  "Given a value, generate a psuedo-random color for visualization"
  [x]
  (def rng (math/rng (hash x)))
  (g/rgb (* 0.7 (math/rng-uniform rng)) (* 0.7 (math/rng-uniform rng)) (* 0.7 (math/rng-uniform rng))))

(defn- calculate-data-bounds
  "Given a data frame, return [min-x max-x min-y max-y]. Use this information for calculating render transform."
  [data x-column y-columns
   override-min-x override-max-x
   override-min-y override-max-y]
  (var min-y math/inf)
  (var max-y math/-inf)
  (def min-x (extreme < (get data x-column)))
  (def max-x (extreme > (get data x-column)))
  (each c y-columns
    (set min-y (min min-y (extreme < (get data c))))
    (set max-y (max max-y (extreme > (get data c)))))
  [(or override-min-x min-x) (or override-max-x max-x)
   (or override-min-y min-y) (or override-max-y max-y)])

(defn- guess-axis-ticks
  "Given a set of numeric values, generate a reasonable array of tick marks given a minimum spacing.
  Biases the tick spacing to a power of 10 (or by 5s) for nicer charts by default"
  [minimum maximum pixel-span min-spacing font &opt no-retry]
  (var max-ticks (math/floor (/ pixel-span min-spacing)))
  (if (zero? max-ticks) (break @[]))
  (def result (array/new max-ticks))
  (var delta (/ (- maximum minimum) max-ticks))
  (var metric minimum)
  # Bias delta towards a power of 10
  (def delta-log10 (math/log10 delta))
  (set delta (math/pow 10 (math/ceil delta-log10)))

  # Allow for steps of 5 as well
  (if (> (- (math/ceil delta-log10) delta-log10) (math/log10 2))
    (*= delta 0.5))
  (def epsilon (* delta 0.001))

  # Get tick metrics
  (set metric (floorn delta metric))
  (while (< metric (- minimum epsilon))
    (+= metric delta))
  (while (< metric (+ epsilon maximum))
    (array/push result metric)
    (+= metric delta))

  # Get a function that will format each tick mark for drawing based on their spacing
  (def formatter
    (if (>= delta 1)
      int-formatter
      (let [fmt-string (string "%." (math/ceil (- (math/log10 delta))) "f")]
            (fn :formatter [x] (string/format fmt-string x)))))

  # Check maximum size of tick text
  (var max-text-width 0)
  (var max-text-height 0)
  (each metric result
    (def [x y] (g/measure-simple-text (formatter metric) font))
    (set max-text-width (max max-text-width x))
    (set max-text-height (max max-text-height y)))

  # Retry if ticks are too close together
  (unless no-retry
    (if (> (+ 10 max-text-width) delta)
      (break (guess-axis-ticks minimum maximum pixel-span (+ 10 max-text-width) font true))))

  [result formatter max-text-width max-text-height])

(defn- draw-frame
  "Draw a frame enclosing a rectangle"
  [image x1 y1 x2 y2 color &opt outer]
  (default outer 1)
  (g/plot-path image [x1 y1 x1 y2 x2 y2 x2 y1] color true)
  (if (> outer 1)
    (draw-frame image (dec x1) (dec y1) (inc x2) (inc y2) color (dec outer))
    image))

###
### API
###

(defn draw-legend
  ```
  Draw a legend given a set of labels and colors
  ```
  [canvas &named background-color font padding color-map labels]
  (def {:width width :height height} (g/unpack canvas))
  (default background-color g/white)
  (def line-color (g/rgb 0.1 0.1 0.1))
  (g/fill-rect canvas 0 0 width height background-color)
  (draw-frame canvas 1 1 (- width 2) (- height 2) line-color 2) # 2 pixel solid frame
  # TODO - measure first, then fill.
  canvas)

(defn draw-axis
  ```
  Draw the axis for the chart. Also return a function that can be used
  to convert a coordinate in the meric space to the screen space.

  * :padding - the number of pixels to leave around all drawn content
  * :font - the font to use for axis text
  * :{x,y}-{min,max} - The bounds for coordinate system to draw

  Returns a tuple [view:gfx2d/Image to-pixel-space:fn to-metric-space:fn]

  * `view` is an image that can be used to draw inside the chart, clipped so you don't overwrite that axes.
  * `(to-pixel-space metric-x metric-y)` converts metric space coordinates to pixel space for plotting on `view`.
  * `(to-metric-space pixel-x pixel-y)` converts pixel coordinates to the metric space.
  ```
  [canvas &named padding font x-min x-max y-min y-max]

  (default padding 10)
  (default font :default)
  (assert x-min)
  (assert x-max)
  (assert y-min)
  (assert y-max)

  (def {:width width :height height} (g/unpack canvas))
  (def line-color (g/rgb 0.1 0.1 0.1))
  (def grid-color (g/rgb 0.8 0.8 0.8))

  (def dx (- x-max x-min))
  (def dy (- y-max y-min))
  (assert (pos? dx))
  (assert (pos? dy))
  (def font-half-height 4)
  (def tick-height 8)

  # TODO - measure required space from x and y legend labels
  (def y-axis-legend-width 25)

  # Allow for more space around final graph
  (def outer-left-padding (+ padding y-axis-legend-width))
  (def outer-right-padding padding)
  (def outer-top-padding padding)
  (def outer-bottom-padding (+ padding 10))
  (def left-padding   (+ outer-left-padding tick-height))
  (def right-padding  outer-right-padding)
  (def top-padding    outer-top-padding)
  (def bottom-padding (+ outer-bottom-padding tick-height))

  # Closure to convert metric space to pixel space
  # TODO - replace with affine transform abstraction
  (def scale-x (/ (- width left-padding right-padding tick-height tick-height) dx))
  (def scale-y (- (/ (- height top-padding bottom-padding tick-height tick-height) dy)))
  (def offset-x (- left-padding (- tick-height) (* scale-x x-min)))
  (def offset-y (- height bottom-padding tick-height (* scale-y y-min)))
  (defn convert
    [metric-x metric-y]
    [(+ offset-x (* scale-x metric-x))
     (+ offset-y (* scale-y metric-y))])

  # Draw X axis
  (def [xticks xformat _ xtextheight] (guess-axis-ticks x-min x-max (- width left-padding right-padding) 20 font))
  (each metric-x xticks
    (def [pixel-x _] (convert metric-x 0))
    (def rounded-pixel-x (math/round pixel-x))
    (def text (xformat metric-x))
    (def [text-width] (g/measure-simple-text text font))
    (g/draw-simple-text canvas (- rounded-pixel-x -1 (* text-width 0.5)) (- height outer-bottom-padding -3) 1 1 text line-color font)
    (g/plot canvas rounded-pixel-x top-padding rounded-pixel-x (- height bottom-padding) grid-color)
    (g/plot canvas rounded-pixel-x (- height outer-bottom-padding) rounded-pixel-x (- height outer-bottom-padding tick-height) line-color))

  # Draw Y axis - unlike X axis, we always expect y axis to be numeric
  (def [yticks yformat ytextwidth] (guess-axis-ticks y-min y-max (- height top-padding bottom-padding) 20 font))
  (each metric-y yticks
    (def [_ pixel-y] (convert 0 metric-y))
    (def rounded-pixel-y (math/round pixel-y))
    (def text (yformat metric-y))
    (def [text-width] (g/measure-simple-text text font))
    (g/draw-simple-text canvas (- outer-left-padding text-width 3) (- rounded-pixel-y font-half-height) 1 1 text line-color font)
    (g/plot canvas left-padding rounded-pixel-y (- width right-padding) rounded-pixel-y grid-color)
    (g/plot canvas outer-left-padding rounded-pixel-y (+ outer-left-padding tick-height) rounded-pixel-y line-color))

  # Draw frame
  (draw-frame canvas left-padding top-padding (- width right-padding) (- height bottom-padding) line-color 2)

  # Create a cropped view inside our "Frame" that can then be used for rendering charts
  (def frame-width (- width left-padding right-padding))
  (def frame-height (- height top-padding bottom-padding))
  (def view (g/viewport canvas
                        (+ 1 left-padding) (+ 1 top-padding)
                        (- frame-width 1) (- frame-height 1)))
  (def scale-x (/ (- frame-width tick-height tick-height) dx))
  (def scale-y (- (/ (- frame-height tick-height tick-height) dy)))
  (def offset-x (- tick-height (* scale-x x-min)))
  (def offset-y (- frame-height -1 tick-height (* scale-y y-min)))
  (defn view-convert
    [metric-x metric-y]
    [(+ offset-x (* scale-x metric-x))
     (+ offset-y (* scale-y metric-y))])
  (defn view-unconvert
    [pixel-x pixel-y]
    [(/ (- pixel-x offset-x) scale-x)
     (/ (- pixel-y offset-y) scale-y)])

  [view view-convert view-unconvert])

(defn line-chart
  "Render a basic line chart. Returns a gfx2d/Image which can be further manipulated with the spork/gfx2d module."
  [&named
   width height data x-spacing y-spacing
   font background-color color-map
   point-radius
   x-min x-max y-min y-max
   padding title
   circle-points
   x-column y-columns]

  (assert x-column)
  (assert y-columns)
  (default width 1280)
  (default height 720)
  (default padding 20)
  (default point-radius 3)
  (default color-map {})
  (default background-color g/white)
  (default font :tall)
  (default circle-points true)
  (def canvas (g/blank width height 4))
  (g/fill-rect canvas 0 0 width height background-color)

  (var title-padding 0)
  (when title
    (def [title-width title-height] (g/measure-simple-text title font))
    (set title-padding (* 2 title-height))
    (g/draw-simple-text canvas (math/round (* 0.5 (- width title-width title-width))) (* 0.5 padding) 2 2 title g/black font))

  # Crop title section out of place where axis and charting will draw
  (def view (g/viewport canvas 0 title-padding width (- height title-padding)))
  (def [x-min x-max y-min y-max] (calculate-data-bounds data x-column y-columns x-min x-max y-min y-max))
  (def [graph-view to-pixel-space to-metric-space]
    (draw-axis view :padding padding :font font
               :x-min x-min :x-max x-max
               :y-min y-min :y-max y-max))

  # Draw graph
  (def xs (get data x-column))
  (each y-column y-columns
    (def graph-color (get color-map y-column (color-hash y-column)))
    (def ys (get data y-column))
    (for i 0 (dec (length xs))
      (def j (+ i 1))
      (def [x1 y1] (map math/round (to-pixel-space (get xs i) (get ys i))))
      (def [x2 y2] (map math/round (to-pixel-space (get xs j) (get ys j))))
      #(g/stroke-path graph-view [x1 y1 x2 y2] graph-color 2)
      (g/plot graph-view x1 y1 x2 y2 graph-color)
      (g/plot graph-view x1 (dec y1) x2 (dec y2) graph-color)
      (g/plot graph-view x1 (inc y1) x2 (inc y2) graph-color)
      (when circle-points
        (when (= 0 i)
          (g/plot-ring graph-view x1 y1 point-radius graph-color))
        (g/plot-ring graph-view x2 y2 point-radius graph-color))))

  canvas)




