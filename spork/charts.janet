###
### This module is for generating graphs and charts on the CPU and rendering
### them to bitmaps. While not completely general in styling, charts should be general
### purpose in visualizing different kinds of data. For more rendering backends or functionality,
### libraries like plPlot may be more suitable.
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

### TODO
### [x] - LABEL YOUR AXES!
### [ ] - wrap colors, padding, font, etc. into some kind of styling table to pass around consistently
### [x] - stippled grid lines
### [ ] - bar chart
### [ ] - heat map
### [ ] - more test charts
### [ ] - pie chart
### [ ] - error bars on line chart
### [ ] - fill between chart
### [ ] - handle nils in y-columns for sparse data
### [ ] - easier custom chart annotations in the metric space (horizontal lines, vertical lines, etc.)

(import spork/gfx2d :as g)

# Defaults
(defdyn *font* "Default font for chart rendering")
(defdyn *text-color* "Default font color for title and axis labels")
(defdyn *stroke-color* "Default color for drawn lines such as frame borders")
(defdyn *background-color* "Default background color for chart rendering")
(defdyn *grid-color* "Default color for grid lines")

(def- default-font :olive)
(def- default-text-color g/black)
(def- default-stroke-color g/black)
(def- default-background-color g/white)
(def- default-grid-color (g/rgb 0.8 0.8 0.8))

(defn- floorn
  "Floor mod n"
  [n x]
  (def x (if (= 0 x) (math/abs x) x)) # no negative 0, messes up rendering!
  (/ (math/floor (* n x)) n))

(defn- ceiln
  "Ceil mod n"
  [n x]
  (def x (if (= 0 x) (math/abs x) x)) # no negative 0, messes up rendering!
  (/ (math/ceil (* n x)) n))

(defn- roundn
  "Round mod n"
  [n x]
  (def x (if (= 0 x) (math/abs x) x)) # no negative 0, messes up rendering!
  (/ (math/round (* n x)) n))

# TODO - bias light or dark depending on background
(defn- color-hash
  "Given a value, generate a pseudo-random color for visualization"
  [x &opt color-seed]
  (def rng (math/rng (hash [x color-seed])))
  (g/rgb (+ 0.2 (* 0.6 (math/rng-uniform rng)))
         (+ 0.2 (* 0.6 (math/rng-uniform rng)))
         (+ 0.2 (* 0.6 (math/rng-uniform rng)))))

(defn- calculate-data-bounds
  "Given a data frame, return [min-x max-x min-y max-y].
  Use this information for calculating render transform. Should handle non-existant columns."
  [data x-column y-columns
   &opt
   width height min-spacing
   override-min-x override-max-x
   override-min-y override-max-y]

  # Calculate precise bounds for all x and y values
  (var min-y math/inf)
  (var max-y math/-inf)
  (var min-x (extreme < (filter identity (get data x-column))))
  (var max-x (extreme > (filter identity (get data x-column))))
  (each c y-columns
    (set min-y (min min-y (extreme < (filter identity (get data c [math/inf])))))
    (set max-y (max max-y (extreme > (filter identity (get data c [math/-inf]))))))

  # Now possibly expand bounds for nice axis ticks in the same way as `guess-axis-ticks`.
  # e.g. [1-99] -> [0-100]
  # Guess delta - making some assumptions is ok since we don't know exact measurements until axes layout and this is just to auto-fit.
  # Better to over-estimate deltas (metric tick spacing) than under-estimate here.
  # Full control is still available to the library user via x-min, x-max, y-min, and y-max.
  (def max-x-ticks (max 1 (math/floor (/ width min-spacing))))
  (def max-y-ticks (max 1 (math/floor (/ height min-spacing))))
  (def x-delta (/ (- max-x min-x) max-x-ticks))
  (def y-delta (/ (- max-y min-y) max-y-ticks))
  (def fudge-factor 1)

  # If minimums are a little over a nice number, set them to the nice number
  (def fudge-min-x (floorn (/ x-delta) min-x))
  (def fudge-min-y (floorn (/ y-delta) min-y))
  (if (< (- min-x fudge-min-x) (* fudge-factor x-delta))
    (set min-x fudge-min-x))
  (if (< (- min-y fudge-min-y) (* fudge-factor y-delta))
    (set min-y fudge-min-y))

  # If the maximums are a little under a nice number, set them to the nice number
  (def fudge-max-x (ceiln (/ x-delta) max-x))
  (def fudge-max-y (ceiln (/ y-delta) max-y))
  (if (< (- fudge-max-x max-x) (* fudge-factor x-delta))
    (set max-x fudge-max-x))
  (if (< (- fudge-max-y max-y) (* fudge-factor y-delta))
    (set max-y fudge-max-y))

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
  (g/plot-path image [x1 y1 x1 y2 x2 y2 x2 y1] color 0 0 true)
  (if (> outer 1)
    (draw-frame image (dec x1) (dec y1) (inc x2) (inc y2) color (dec outer))
    image))

###
### API
###

(defn dark-mode
  ```
  Set dynamic color defaults to dark mode
  ```
  []
  (setdyn *background-color* g/black)
  (setdyn *grid-color* (g/rgb 0.2 0.2 0.2))
  (setdyn *stroke-color* g/white)
  (setdyn *text-color* g/white))

(defn light-mode
  ```
  Set dynamic color defaults to light mode
  ```
  []
  (setdyn *background-color* g/white)
  (setdyn *grid-color* (g/rgb 0.8 0.8 0.8))
  (setdyn *stroke-color* g/black)
  (setdyn *text-color* g/black))

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
   horizontal frame color-seed legend-map line-color text-color]
  (default font :default)
  (default padding 16)
  (default color-map {})
  (default legend-map {})
  (default background-color (dyn *background-color* default-background-color))
  # TODO pass in configured colors
  (default line-color (dyn *grid-color* default-grid-color))
  (default text-color (dyn *text-color* default-text-color))
  (when canvas
    (def {:width width :height height} (g/unpack canvas))
    (when frame (g/fill-rect canvas 0 0 width height background-color)))
  (def label-height (let [[_ h] (g/measure-simple-text "Mg" font)] h))
  (def swatch-size label-height)
  (def spacing (+ label-height padding 1))
  (def small-spacing (math/round (* 0.125 label-height)))
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
      (g/draw-simple-text canvas (+ x swatch-size padding) (+ small-spacing y) label text-color font))
    (def item-width (+ padding padding padding text-width swatch-size))
    (set max-width (max max-width item-width))
    (if horizontal
      (+= x (+ item-width padding))
      (+= y spacing)))
  (+= y (+ 1 padding))
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
  to convert a coordinate in the metric space to the screen space. Most parameters
  are optional with sane defaults, but canvas, x-min, x-max, y-min, y-max are all required.

  * :x-label - optional label for the x axis
  * :y-label - optional label for the y axis
  * :padding - the number of pixels to leave around all drawn content
  * :font - the font to use for axis text
  * :{x,y}-{min,max} - The bounds for coordinate system to draw
  * :grid - Style for drawing grid-lines. Can be nil (none), :none, :solid, or :stipple
  * :format-x - unary function (fn [x] ...) that returns a string to label x axis tick marks with
  * :format-y - unary function (fn [y] ...) that returns a string to label y axis tick marks with
  * :x-prefix - if format-x not provided, allows easily adding a string prefix to x axis tick mark labels
  * :y-prefix - if format-y not provided, allows easily adding a string prefix to y axis tick mark labels
  * :x-suffix - if format-x not provided, allows easily adding a string suffix to x axis tick mark labels
  * :y-suffix - if format-y not provided, allows easily adding a string suffix to y axis tick mark labels
  * :x-ticks - Allow setting specific tick marks to be used marking the x axis rather than making a guess.
  * :x-minor-ticks - How many minor tick marks, if any, to place between major tick marks on the x axis
  * :y-minor-ticks - How many minor tick marks, if any, to place between major tick marks on the y axis

  Returns a tuple [view:gfx2d/Image to-pixel-space:fn to-metric-space:fn]

  * `view` is an image that can be used to draw inside the chart, clipped so you don't overwrite that axes.
  * `(to-pixel-space metric-x metric-y)` converts metric space coordinates to pixel space for plotting on `view`.
  * `(to-metric-space pixel-x pixel-y)` converts pixel coordinates to the metric space.
  ```
  [canvas &named padding font
   x-min x-max y-min y-max
   grid format-x format-y
   x-label y-label
   x-suffix x-prefix y-suffix y-prefix
   x-ticks x-minor-ticks y-minor-ticks]

  (default padding 8)
  (default font :default)
  (assert canvas)
  (assert x-min)
  (assert x-max)
  (assert y-min)
  (assert y-max)

  (def {:width width :height height} (g/unpack canvas))
  (def line-color (dyn *stroke-color* default-stroke-color))
  (def grid-color (dyn *grid-color* default-grid-color))

  (def dx (- x-max x-min))
  (def dy (- y-max y-min))
  (assert (pos? dx))
  (assert (pos? dy))
  (def font-height (let [[_ h] (g/measure-simple-text "Mg" font)] h))
  (def font-half-height (div font-height 2))
  (def tick-height 8)
  (def has-grid (not= grid :none))
  (def stipple-cycle (if (= grid :stipple) 8 0))
  (def stipple-on 4)

  # Calculate top and bottom padding
  (def outer-top-padding padding)
  (def outer-bottom-padding (+ padding font-height (if x-label (+ padding font-height) 0)))
  (def top-padding outer-top-padding)
  (def bottom-padding (+ outer-bottom-padding tick-height))

  # Draw X Label
  (when x-label
    (def [w _h] (g/measure-simple-text x-label))
    (def yy (- height padding font-height))
    (g/draw-simple-text canvas (div (- width w) 2) yy x-label line-color font))

  # Guess y axis ticks - used to calculate left and right padding
  (def [yticks yformat y-axis-tick-label-width]
    (guess-axis-ticks y-min y-max (- height top-padding bottom-padding) 20 true font y-prefix y-suffix format-y))

  # Calculate left and right padding once y-axis is guessed
  (def outer-left-padding (+ padding y-axis-tick-label-width (if y-label (+ padding font-height) 0)))
  (def outer-right-padding outer-left-padding) # make it symetrical, looks much nicer
  (def left-padding (+ outer-left-padding tick-height))
  (def right-padding outer-right-padding)

  # Draw Y Label
  (when y-label
    (def [w _h] (g/measure-simple-text y-label))
    (g/draw-simple-text canvas padding (div (+ height w top-padding (- bottom-padding)) 2) y-label line-color font 1 1 1))

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
    (g/draw-simple-text canvas (- outer-left-padding text-width 3) (- rounded-pixel-y font-half-height) text line-color font)
    (if has-grid
      (g/plot canvas left-padding rounded-pixel-y (- width right-padding) rounded-pixel-y grid-color stipple-cycle stipple-on)
      (g/plot canvas outer-left-padding rounded-pixel-y (+ outer-left-padding tick-height) rounded-pixel-y grid-color)))

  # Draw X axis - allow manual override for x tick marks
  (def [xticks xformat]
    (if x-ticks [x-ticks (if format-x format-x string)]
      (guess-axis-ticks x-min x-max (- width left-padding right-padding) 20 false font x-prefix x-suffix format-x)))
  (each metric-x xticks
    (def [pixel-x _] (convert metric-x 0))
    (def rounded-pixel-x (math/round pixel-x))
    (def text (xformat metric-x))
    (def [text-width] (g/measure-simple-text text font))
    (g/draw-simple-text canvas (- rounded-pixel-x -1 (* text-width 0.5)) (- height outer-bottom-padding -3) text line-color font)
    (if has-grid
      (g/plot canvas rounded-pixel-x top-padding rounded-pixel-x (- height bottom-padding) grid-color stipple-cycle stipple-on)
      (g/plot canvas rounded-pixel-x (- height outer-bottom-padding) rounded-pixel-x (- height outer-bottom-padding tick-height) grid-color)))

  # Draw minor x tick marks
  (when (and x-minor-ticks (< 1 (length xticks)))
    (def len (length xticks))
    (def dx-first (- (in xticks 1) (in xticks 0)))
    (def dx-last (- (in xticks (- len 1)) (in xticks (- len 2))))
    # we must draw minor ticks before and after the first and last major ticks until the edge of the axis
    (def padded-ticks [(- (in xticks 0) dx-first) ;xticks (+ (in xticks (- len 1)) dx-last)])
    (loop [j :range [1 (length padded-ticks)]
           :let [i (- j 1)
                 t0x (in padded-ticks i)
                 t1x (in padded-ticks j)
                 dx (- t1x t0x)]
           xfloat :range [t0x (+ t1x 0.00001) (/ dx x-minor-ticks)]
           :let [x (math/round (first (convert xfloat 0)))]
           :when (and (> x left-padding) (< x (- width right-padding)))]
      (g/plot canvas x (- height outer-bottom-padding tick-height) x (- height outer-bottom-padding (div tick-height 2)) grid-color)))

  # Draw minor y tick marks
  (when (and y-minor-ticks (< 1 (length yticks)))
    (def len (length yticks))
    (def dy-first (- (in yticks 1) (in yticks 0)))
    (def dy-last (- (in yticks (- len 1)) (in yticks (- len 2))))
    # we must draw minor ticks before and after the first and last major ticks until the edge of the axis
    (def padded-ticks [(- (in yticks 0) dy-first) ;yticks (+ (in yticks (- len 1)) dy-last)])
    (loop [j :range [1 (length padded-ticks)]
           :let [i (- j 1)
                 t0y (in padded-ticks i)
                 t1y (in padded-ticks j)
                 dy (- t1y t0y)]
           yfloat :range [t0y (+ t1y 0.00001) (/ dy y-minor-ticks)]
           :let [y (math/round (get (convert 0 yfloat) 1))]
           :when (and (> y top-padding) (< y (- height bottom-padding)))]
      (g/plot canvas (+ outer-left-padding (div tick-height 2)) y (+ outer-left-padding tick-height) y grid-color)))

  # Draw frame
  (draw-frame canvas left-padding top-padding (- width right-padding) (- height bottom-padding) grid-color 2)

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

  Basic Parameters
  * :width - canvas width
  * :height - canvas height
  * :data - a data frame to use for data
  * :title - an optional title to add to the rendered image
  * :font - font used to draw text, including title, legend, and axes labels
  * :save-as - save the generated image to file. Can be any format supported by the gfx2d module
  * :x-column - the name of the data frame column to use for the x axis
  * :y-columns - a list of column names to use for the chart
  * :x-ticks - manually set the tick marks on the X axis instead of auto-detecting them

  Axes Styling
  * :x-label - optional label for the x axis
  * :y-label - optional label for the y axis
  * :grid - set to true to turn on drawing grid lines
  * :x-suffix - add a string suffix to each tick label on the x-axis
  * :y-suffix - add a string suffix to each tick label on the x-axis
  * :x-prefix - add a string prefix to each tick label on the y-axis
  * :y-prefix - add a string prefix to each tick label on the y-axis
  * :x-minor-ticks - how many, if any, small ticks to add between each large tick mark on the x axis
  * :y-minor-ticks - how many, if any, small ticks to add between each large tick mark on the y axis

  Chart Styling
  * :padding - the number of pixels of white space around various elements of the chart
  * :background-color - color of background, defaults to white
  * :text-color - color of text, defaults to black
  * :color-map - a dictionary mapping columns to colors. By default will hash column name to pseudo-random colors
  * :color-seed - an optional seed to change the default colors chosen by the color hash
  * :scatter - set to true to disable lines connecting points
  * :legend - set to true to add a legend to the top of the chart
  * :legend-map - a dictionary mapping column names to pretty text for the chart
  * :point-radius - radius of points when drawing a scatter plot
  * :line-stype - how to actually draw lines. Can be one of :stroke, :plot, or :stipple. Default is :plot.

  Axis Boundaries
  * :x-min - minimum x coordinate on chart
  * :x-max - maximum x coordinate on chart
  * :y-min - minimum y coordinate on chart
  * :y-max - maximum y coordinate on chart
  ```
  [&named
   width height data
   font background-color text-color color-map
   point-radius
   x-min x-max y-min y-max
   padding title
   circle-points
   scatter grid legend
   format-x format-y
   color-seed
   save-as
   legend-map
   line-style
   x-label y-label
   x-suffix x-prefix y-suffix y-prefix
   x-column y-columns
   x-ticks x-minor-ticks y-minor-ticks]

  # Check parameters and set defaults.
  (assert x-column)
  (assert y-columns)
  (default width 1280)
  (default height 720)
  (default padding 16)
  (default point-radius 3)
  (default color-map {})
  (default background-color (dyn *background-color* default-background-color))
  (default text-color (dyn *text-color* default-text-color))
  (default font (dyn *font* default-font))
  (default circle-points false)
  (default grid :none)
  (default line-style :plot)

  # Get canvas
  (def canvas (g/blank width height 4))
  (g/fill-rect canvas 0 0 width height background-color)

  # Render title section, and update view to cut out title
  (var title-padding 0)
  (when title
    (def [title-width title-height] (g/measure-simple-text title font 2 2))
    (set title-padding (+ padding title-height))
    (g/draw-simple-text canvas (math/round (* 0.5 (- width title-width))) padding title text-color font 2 2))

  # Add legend if legend = :top. This makes a horizontal legend just below the title with no extra framing
  (when (or (= legend true) (= legend :top))
    (+= title-padding (div padding 2))
    (def [lw lh] (draw-legend nil :font font :padding 4 :horizontal true :labels y-columns :legend-map legend-map))
    (def legend-view (g/viewport canvas (math/floor (* (- width lw) 0.5)) title-padding lw lh true))
    (+= title-padding lh)
    (-= title-padding (math/floor (* 0.5 padding))) # just looks a bit better
    (draw-legend legend-view :font font :padding 4 :horizontal true :labels y-columns :color-map
                 color-map :color-seed color-seed :legend-map legend-map :text-color text-color))

  # Crop title section out of place where axis and charting will draw
  (def view (g/viewport canvas 0 title-padding width (- height title-padding)))

  # Draw axes
  (def [x-min x-max y-min y-max]
    (let [{:width view-width :height view-height} (g/unpack view)]
      (calculate-data-bounds data x-column y-columns
                             view-width view-height 20
                             x-min x-max y-min y-max)))
  (def [graph-view to-pixel-space _to-metric-space]
    (draw-axes view
               :padding padding :font font
               :grid grid
               :format-x format-x :format-y format-y
               :x-suffix x-suffix :x-prefix x-prefix
               :y-suffix y-suffix :y-prefix y-prefix
               :x-min x-min :x-max x-max
               :y-min y-min :y-max y-max
               :x-ticks x-ticks
               :x-label x-label :y-label y-label
               :x-minor-ticks x-minor-ticks
               :y-minor-ticks y-minor-ticks))

  # Draw graph
  (def xs (get data x-column))
  (each y-column (reverse y-columns) # draw first listed on top
    (def graph-color (get color-map y-column (color-hash y-column color-seed)))
    (def ys (get data y-column))

    # Collect points - handle missing ys
    (def pts @[])
    (for i 0 (length xs)
      (def x (get xs i))
      (when x
        (def y (get ys i))
        (when y
          (def [x1 y1] (to-pixel-space x y))
          (array/push pts (math/round x1) (math/round y1)))))

    # TODO - more configurable stroke style
    (def line-style2 (if (dictionary? line-style) (get line-style y-column :plot) line-style))
    (case line-style2
      :stipple
      (do
        (def up-pts (array/slice pts))
        (loop [i :range [1 (length pts) 2]]
          (+= (up-pts i) 1))
        (g/plot-path graph-view up-pts graph-color 8 5)
        (g/plot-path graph-view pts graph-color 8 5))
      :plot
      (do
        (def up-pts (array/slice pts))
        (loop [i :range [1 (length pts) 2]]
          (+= (up-pts i) 1))
        (g/plot-path graph-view pts graph-color)
        (g/plot-path graph-view up-pts graph-color))
      :stroke
      (do
        (g/stroke-path graph-view pts graph-color 1)))

    (when (or scatter circle-points)
      (loop [i :range [0 (length pts) 2]]
        (def x (get pts i))
        (def y (get pts (+ 1 i)))
        (g/plot-ring graph-view x y point-radius graph-color))))

  # TODO - draw legend in inner corners
  (when (index-of legend [:top-left :top-right :bottom-left :bottom-right])
    (def [lw lh] (draw-legend nil :font font :padding 4 :labels y-columns :legend-map legend-map :frame true))
    (def {:width gw :height gh} (g/unpack graph-view))
    (def legend-view
      (case legend
        :top-left (g/viewport graph-view padding padding lw lh true)
        :top-right (g/viewport graph-view (- gw lw padding) padding lw lh true)
        :bottom-left (g/viewport graph-view padding (- gh lh padding) lw lh true)
        :bottom-right (g/viewport graph-view (- gw lw padding) (- gh lh padding) lw lh true)))
    (g/fill-rect legend-view 0 0 lw lh background-color)
    (draw-legend legend-view :font font :padding 4 :labels y-columns
                 :color-map color-map :color-seed color-seed :legend-map legend-map :frame true))

  (when save-as
    (g/save save-as canvas))

  canvas)
