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

(import ./path)
(import spork/gfx2d :as g)

# Defaults
# TODO - use these
(defdyn *default-font* "Default font for chart rendering")
(defdyn *default-text-color* "Default font color for title and axis labels")
(defdyn *default-background-color* "Default background color for chart rendering")
(defdyn *default-grid-color* "Default color for grid lines")

(def- text-color g/black)
(def- paper-color g/white)
(def- grid-color (g/rgb 0.8 0.8 0.8))

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

# TODO - bias light or dark depending on background
(defn- color-hash
  "Given a value, generate a psuedo-random color for visualization"
  [x &opt color-seed]
  (def rng (math/rng (hash [x color-seed])))
  (g/rgb (+ 0.2 (* 0.6 (math/rng-uniform rng)))
         (+ 0.2 (* 0.6 (math/rng-uniform rng)))
         (+ 0.2 (* 0.6 (math/rng-uniform rng)))))

(defn- calculate-data-bounds
  "Given a data frame, return [min-x max-x min-y max-y].
  Use this information for calculating render transform. Should handle non-existant columns."
  [data x-column y-columns
   override-min-x override-max-x
   override-min-y override-max-y]
  # TODO - expand bounds for nice axis ticks in the same way as `guess-axis-ticks`.
  # e.g. [1-99] -> [0-100]
  (var min-y math/inf)
  (var max-y math/-inf)
  (def min-x (extreme < (get data x-column)))
  (def max-x (extreme > (get data x-column)))
  (each c y-columns
    (set min-y (min min-y (extreme < (get data c [math/inf]))))
    (set max-y (max max-y (extreme > (get data c [math/-inf])))))
  [(or override-min-x min-x) (or override-max-x max-x)
   (or override-min-y min-y) (or override-max-y max-y)])

(defn- guess-axis-ticks
  "Given a set of numeric values, generate a reasonable array of tick marks given a minimum spacing.
  Biases the tick spacing to a power of 10 (or by 5s) for nicer charts by default"
  [minimum maximum pixel-span min-spacing vertical font prefix suffix &opt force-formatter no-retry] # TODO - too many unnamed arguments
  (default suffix "")
  (default prefix "")
  (var max-ticks (math/floor (/ pixel-span min-spacing)))
  (if (zero? max-ticks) (break @[]))
  (def result (array/new max-ticks))
  (var delta (/ (- maximum minimum) max-ticks))
  (var metric minimum)

  # Bias delta towards a power of 10 for nice tick intervals
  # TODO - allow for other bases
  (def delta-log10 (math/log10 delta))
  (set delta (math/pow 10 (math/ceil delta-log10)))

  # Allow for steps of 5 as well
  (if (> (- (math/ceil delta-log10) delta-log10) (math/log10 2))
    (*= delta 0.5))
  (def epsilon (* delta 0.001))

  # Get tick metrics
  (set metric (floorn (/ delta) metric))
  (while (< metric (- minimum epsilon))
    (+= metric delta))
  (while (<= metric (+ epsilon maximum))
    (array/push result metric)
    (+= metric delta))

  # Get a function that will format each tick mark for drawing based on their spacing
  (def formatter
    (or force-formatter
        (if (>= delta 1)
          (fn :formatter-int [x] (string/format "%s%d%s" prefix (math/round x) suffix))
          (let [fmt-string (string "%s%." (math/ceil (- (math/log10 delta))) "f%s")]
            (fn :formatter [x] (string/format fmt-string prefix x suffix))))))

  # Check maximum size of tick text
  (var max-text-width 0)
  (var max-text-height 0)
  (each metric result
    (def [x y] (g/measure-simple-text (formatter metric) font))
    (set max-text-width (max max-text-width x))
    (set max-text-height (max max-text-height y)))
  (def min-spacing (+ 10 (if vertical max-text-height max-text-width)))

  # Retry if ticks are too close together
  (unless no-retry
    (if (> (+ 10 min-spacing) delta)
      (break (guess-axis-ticks minimum maximum pixel-span min-spacing vertical font prefix suffix force-formatter true))))

  # TODO - use text boundaries to set padding
  [result formatter max-text-width max-text-height])

(defn- draw-frame
  "Draw a frame enclosing a rectangle that is `outer` pixels wide"
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

  `canvas` can be either nil to skip drawing or a gfx2d/Image.

  * :background-color - the color of the background of the legend
  * :font - the font to use for legend text
  * :padding - the number of pixels to leave around all drawn content
  * :color-map - a map of labels to colors
  * :labels - a list of labels to draw in the legend

  Return [w h] of the area that was or would be drawn if the legend were to be drawn.
  ```
  [canvas &named
   background-color font padding color-map labels
   horizontal frame color-seed legend-map]
  (default font :default)
  (default padding 8)
  (default color-map {})
  (default legend-map {})
  (default background-color paper-color)
  (def line-color text-color)
  (when canvas
    (def {:width width :height height} (g/unpack canvas))
    (when frame (g/fill-rect canvas 0 0 width height background-color)))
  (def label-height (let [[_ h] (g/measure-simple-text "Mg" font)] h))
  (def swatch-size (* 1 label-height))
  (def spacing (math/ceil (* 1.5 label-height)))
  (def padding (if frame (+ padding 2) padding)) # add frame border
  (var y padding)
  (var x padding)
  (var max-width 0)
  (each i labels
    (def label (string (get legend-map i i)))
    (def [text-width _] (g/measure-simple-text label font))
    (when canvas
      (def color (get color-map i (color-hash i color-seed)))
      (g/fill-rect canvas x y (+ x swatch-size) (+ y swatch-size) color)
      (g/draw-simple-text canvas (+ x swatch-size 3) y 1 1 label color font))
    (def item-width (+ padding padding text-width swatch-size))
    (set max-width (max max-width item-width))
    (if horizontal
      (+= x (+ spacing text-width padding))
      (+= y spacing)))
  (+= y padding)
  (+= x padding)
  (when (and canvas frame)
    (def {:width width :height height} (g/unpack canvas))
    (draw-frame canvas 1 1 (- width 2) (- height 2) line-color 2)) # 2 pixel solid frame
  (if horizontal
    [x (+ label-height y)]
    [(+ padding max-width) (+ y label-height (- spacing))]))

(defn draw-axes
  ```
  Draw the axis for the chart. Also return a function that can be used
  to convert a coordinate in the meric space to the screen space. Most parameters
  are optional with sane defaults, but canvas, x-min, x-max, y-min, y-max are all required.

  * :padding - the number of pixels to leave around all drawn content
  * :font - the font to use for axis text
  * :{x,y}-{min,max} - The bounds for coordinate system to draw
  * :grid - Boolean to enable drawing grid-lines.
  * :format-x - unary function (fn [x] ...) that returns a string to label x axis tick marks with
  * :format-y - unary function (fn [y] ...) that returns a string to label y axis tick marks with
  * :x-prefix - if format-x not provided, allows easily adding a string prefix to x axis tick mark labels
  * :y-prefix - if format-y not provided, allows easily adding a string prefiox to y axis tick mark labels
  * :x-suffix - if format-x not provided, allows easily adding a string suffix to x axis tick mark labels
  * :y-suffix - if format-y not provided, allows easily adding a string suffix to y axis tick mark labels
  * :x-ticks - Allow setting specific tick marks to be used marking the x axis rather than making a guess.

  Returns a tuple [view:gfx2d/Image to-pixel-space:fn to-metric-space:fn]

  * `view` is an image that can be used to draw inside the chart, clipped so you don't overwrite that axes.
  * `(to-pixel-space metric-x metric-y)` converts metric space coordinates to pixel space for plotting on `view`.
  * `(to-metric-space pixel-x pixel-y)` converts pixel coordinates to the metric space.
  ```
  [canvas &named padding font
   x-min x-max y-min y-max
   grid format-x format-y
   x-suffix x-prefix y-suffix y-prefix
   x-ticks]

  (default padding 10)
  (default font :default)
  (assert canvas)
  (assert x-min)
  (assert x-max)
  (assert y-min)
  (assert y-max)

  (def {:width width :height height} (g/unpack canvas))
  (def line-color text-color)

  (def dx (- x-max x-min))
  (def dy (- y-max y-min))
  (assert (pos? dx))
  (assert (pos? dy))
  (def font-half-height 4)
  (def tick-height 8)

  # Calculate top and bottom padding
  (def outer-top-padding padding)
  (def outer-bottom-padding (+ padding 10))
  (def top-padding outer-top-padding)
  (def bottom-padding (+ outer-bottom-padding tick-height))

  # Guess y axis ticks - used to calculate left and right padding
  (def [yticks yformat y-axis-tick-label-width]
    (guess-axis-ticks y-min y-max (- height top-padding bottom-padding) 20 true font y-prefix y-suffix format-y))

  # Calculate left and right padding once y-axis is guessed
  (def outer-left-padding (+ padding y-axis-tick-label-width))
  (def outer-right-padding padding)
  (def left-padding (+ outer-left-padding tick-height))
  (def right-padding outer-right-padding)

  # Closure to convert metric space to pixel space - only can be done after full padding calculations
  (def scale-x (/ (- width left-padding right-padding tick-height tick-height) dx))
  (def scale-y (- (/ (- height top-padding bottom-padding tick-height tick-height) dy)))
  (def offset-x (- left-padding (- tick-height) (* scale-x x-min)))
  (def offset-y (- height bottom-padding tick-height (* scale-y y-min)))
  (defn convert
    [metric-x metric-y]
    [(+ offset-x (* scale-x metric-x))
     (+ offset-y (* scale-y metric-y))])

  # Draw Y axis
  (each metric-y yticks
    (def [_ pixel-y] (convert 0 metric-y))
    (def rounded-pixel-y (math/round pixel-y))
    (def text (yformat metric-y))
    (def [text-width] (g/measure-simple-text text font))
    (g/draw-simple-text canvas (- outer-left-padding text-width 3) (- rounded-pixel-y font-half-height) 1 1 text line-color font)
    (when grid (g/plot canvas left-padding rounded-pixel-y (- width right-padding) rounded-pixel-y grid-color))
    (g/plot canvas outer-left-padding rounded-pixel-y (+ outer-left-padding tick-height) rounded-pixel-y line-color))

  # Draw X axis - allow manual override for x tick marks
  (def [xticks xformat]
    (if x-ticks [x-ticks (if format-x format-x string)]
      (guess-axis-ticks x-min x-max (- width left-padding right-padding) 20 false font x-prefix x-suffix format-x)))
  (each metric-x xticks
    (def [pixel-x _] (convert metric-x 0))
    (def rounded-pixel-x (math/round pixel-x))
    (def text (xformat metric-x))
    (def [text-width] (g/measure-simple-text text font))
    (g/draw-simple-text canvas (- rounded-pixel-x -1 (* text-width 0.5)) (- height outer-bottom-padding -3) 1 1 text line-color font)
    (when grid (g/plot canvas rounded-pixel-x top-padding rounded-pixel-x (- height bottom-padding) grid-color))
    (g/plot canvas rounded-pixel-x (- height outer-bottom-padding) rounded-pixel-x (- height outer-bottom-padding tick-height) line-color))

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
  ```
  Render a line chart. Returns a gfx2d/Image which can be further manipulated with the spork/gfx2d module.


  ```
  [&named
   width height data x-spacing y-spacing
   font background-color color-map
   point-radius
   x-min x-max y-min y-max
   padding title
   circle-points
   scatter grid legend
   format-x format-y
   color-seed
   save-as
   legend-map
   x-suffix x-prefix y-suffix y-prefix
   x-column y-columns
   x-ticks]

  # Check parameters and set defaults.
  (assert x-column)
  (assert y-columns)
  (default width 1280)
  (default height 720)
  (default padding 20)
  (default point-radius 3)
  (default color-map {})
  (default background-color paper-color)
  (default font :tall)
  (default circle-points false)
  (default grid false)

  # Get canvas
  (def canvas (g/blank width height 4))
  (g/fill-rect canvas 0 0 width height background-color)

  # Render title section, and update view to cut out title
  (var title-padding 0)
  (when title
    (def [title-width title-height] (g/measure-simple-text title font))
    (set title-padding (* 2 title-height))
    (g/draw-simple-text canvas (math/round (* 0.5 (- width title-width title-width))) padding 2 2 title text-color font))

  # Add legend if legend = :top. This makes a horizontal legend just below the title with no extra framing
  (when (or (= legend true) (= legend :top))
    (+= title-padding padding)
    (def [lw lh] (draw-legend nil :font font :padding 4 :horizontal true :labels y-columns :legend-map legend-map))
    (def legend-view (g/viewport canvas (math/floor (* (- width lw) 0.5)) title-padding lw lh))
    (+= title-padding lh)
    (-= title-padding (math/floor (* 0.5 padding))) # just looks a bit better
    (draw-legend legend-view :font font :padding 4 :horizontal true :labels y-columns :color-map color-map :color-seed color-seed :legend-map legend-map))

  # Crop title section out of place where axis and charting will draw
  (def view (g/viewport canvas 0 title-padding width (- height title-padding)))

  # Draw axes
  (def [x-min x-max y-min y-max] (calculate-data-bounds data x-column y-columns x-min x-max y-min y-max))
  (def [graph-view to-pixel-space to-metric-space]
    (draw-axes view :padding padding :font font
               :grid grid
               :format-x format-x :format-y format-y
               :x-suffix x-suffix :x-prefix x-prefix
               :y-suffix y-suffix :y-prefix y-prefix
               :x-min x-min :x-max x-max
               :y-min y-min :y-max y-max
               :x-ticks x-ticks))

  # Draw graph
  (def xs (get data x-column))
  (each y-column y-columns
    (def graph-color (get color-map y-column (color-hash y-column color-seed)))
    (def ys (get data y-column))
    (for i 0 (dec (length xs))
      (def j (+ i 1))
      (def [x1 y1] (map math/round (to-pixel-space (get xs i) (get ys i))))
      (def [x2 y2] (map math/round (to-pixel-space (get xs j) (get ys j))))
      (unless scatter
        #(g/stroke-path graph-view [x1 y1 x2 y2] graph-color 2)
        (g/plot graph-view x1 y1 x2 y2 graph-color)
        (g/plot graph-view x1 (dec y1) x2 (dec y2) graph-color)
        (g/plot graph-view x1 (inc y1) x2 (inc y2) graph-color))
      (when (or scatter circle-points)
        (when (= 0 i)
          (g/plot-ring graph-view x1 y1 point-radius graph-color))
        (g/plot-ring graph-view x2 y2 point-radius graph-color))))

  # TODO - draw legend in inner corners

  (when save-as
    (g/save save-as canvas))

  canvas)
