###
### charts.janet
###
### NOTE: Beta-quality - apis may change.
###
### This module is for generating graphs and charts on the CPU and rendering
### them to bitmaps. While not completely general in styling, charts should be general
### purpose in visualizing different kinds of data. For more rendering backends or functionality,
### libraries like plPlot may be more suitable. However, out-of-the-box chart generation with minimal
### dependencies is very useful to have.
###
### Data is passed to most charts as a "data-frame", which is a table mapping keyword (or any Janet value) column names
### to arrays of data points, usually numbers.
###
### Data frame example:
###
### {:timestamp [1 2 3 4 5 6]
###  :temperature-1 [75.1 75.2 75.4 75.5 75.5 75.4]
###  :temperature-2 [55.1 55.4 55.7 60.0 60.4 60.9]}
###
### By default, most charts will not do any anti-aliasing with the default styles and fonts. However, anti-aliased TTF
### fonts are supported in any place where we accept a font, and all charts that benefit from it support super-sample
### anti-aliasing for chart graphics. This allows for both "pixel-art" style charts for low resolution bit maps as well
### as publication quality charts ready for high-DPI screens. Care is taken to avoid anti-aliasing when approriate by
### using pixel boundaries as edges.
###
### Features!
###
### [x] - horizontal legend should still be able to wrap vertically if too wide.
### [x] - LABEL YOUR AXES!
### [x] - wrap colors, padding, font, etc. into some kind of styling table to pass around consistently
### [x] - stippled grid lines
### [x] - bar chart
### [x] - area chart
### [x] - horizontal bar charts
### [ ] - multi-bar charts
### [ ] - flame graph
### [ ] - packing chart (alternative to pie-charts)
### [x] - heat map
### [ ] - more graphics for scatter plots besides rings.
### [ ] - error bars on line chart
### [ ] - fill between chart
### [ ] - attributed text for captions and annotations
### [x] - handle nils in y-columns for sparse data
### [x] - easier custom chart annotations in the metric space (horizontal lines, vertical lines, etc.)
### [ ] - captions and sub-titles
### [ ] - labeling data points

(import spork/gfx2d :as g)

# Defaults
(defdyn *font* "Default font for chart rendering")
(defdyn *text-color* "Default font color for title and axis labels")
(defdyn *stroke-color* "Default color for drawn lines such as frame borders")
(defdyn *background-color* "Default background color for chart rendering")
(defdyn *grid-color* "Default color for grid lines")
(defdyn *padding* "Default padding for charts")
(defdyn *color-seed* "Random seed to use when picking pseudo-random colors for charts")

# Default defaults
(def- default-font :olive)
(def- default-text-color g/black)
(def- default-stroke-color g/black)
(def- default-background-color g/white)
(def- default-grid-color (g/rgb 0.8 0.8 0.8))
(def- default-padding 16)
(def- default-width 1280)
(def- default-height 720)

(defn- check-enum-impl
  "Assert that a value x is in options, and give a nice error if not"
  [arg-name x options optstring]
  (assert (get options x) (string/format "expected argument %v to be one of %s, got %v" arg-name optstring x)))

(defmacro- enum
  [x & options]
  "Shorthand to assert that a value x is in options, and give a nice error if not"
  (def quote-options (invert options))
  (def optstring (string/join (map describe options) ", "))
  ~(,check-enum-impl ',x ,x ',quote-options ,optstring))

(defn- draw-frame
  "Draw a frame enclosing a rectangle that is `outer` pixels wide"
  [image x1 y1 x2 y2 color &opt outer]
  (default outer 1)
  (g/plot-path image [x1 y1 x1 y2 x2 y2 x2 y1] color 0 0 true)
  (if (> outer 1)
    (draw-frame image (dec x1) (dec y1) (inc x2) (inc y2) color (dec outer))
    image))

(defn- text-measure
  "Measure text either using simple text or a TTF font"
  [text &opt font scale orientation]
  (default font :default)
  (default scale 1)
  (default orientation 0)
  (if (abstract? font)
    (g/measure-text font text scale orientation)
    (g/measure-simple-text text font scale scale orientation)))

(defn- text-draw
  "Draw text either using simple text or a TTF font."
  [image x y text color &opt font scale orientation]
  (default font :default)
  (default scale 1)
  (default orientation 0)
  # Uncomment to check text bounding boxes for layout calculations
  # (def [w h] (text-measure text font scale orientation))
  # (draw-frame image x y ((if (> orientation 1) - +) x w) ((if (even? orientation) + -) y h) color)
  (if (abstract? font)
    (g/draw-text image font x y text color scale orientation)
    (g/draw-simple-text image x y text color font scale scale orientation)))

(defn- floorn
  "Floor mod n"
  [n x]
  (def x :shadow (if (= 0 x) (math/abs x) x)) # no negative 0, messes up rendering!
  (* (math/floor (/ x n)) n))

(defn- ceiln
  "Ceil mod n"
  [n x]
  (def x :shadow (if (= 0 x) (math/abs x) x)) # no negative 0, messes up rendering!
  (* (math/ceil (/ x n)) n))

(defn- color-value
  "Gray scale value of a color"
  [c]
  (def [r g b a] (g/as-srgb c))
  (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))

(defn- lerp [x y t] (+ (* x t) (* y (- 1 t))))
(defn- clamp [x a b] (cond (< x a) a (< b x) b x))

(defn- canvas-and-dimensions
  "Get a canvas and dimensions given canvas, width and height, where canvas or width and height can be nil."
  [canvas width height]
  (default canvas (g/blank (or width default-width) (or height default-height) 4))
  (def {:width w :height h} (g/unpack canvas))
  (if width (assert (= w width) "width does not match provided canvas width"))
  (if height (assert (= h height) "height does not match provided canvas height"))
  [canvas w h])

###
### Graph Axes Calculation and rendering
###

(defn- calculate-data-bounds
  "Given a data frame, return [min-x max-x min-y max-y].
  Use this information for calculating render transform. Should handle non-existant columns."
  [data x-column y-columns
   &opt
   override-min-x override-max-x
   override-min-y override-max-y]

  # Just skip all the guesswork
  (when (and override-min-x override-max-x override-min-y override-max-y)
    (break [override-min-x override-max-x override-min-y override-max-y]))

  # Calculate precise bounds for all x and y values
  (var min-x math/inf)
  (var max-x math/-inf)
  (each c (if (indexed? x-column) x-column [x-column])
    (set min-x (min min-x (extreme < (filter identity (get data c [0])))))
    (set max-x (max max-x (extreme > (filter identity (get data c [1]))))))
  (var min-y math/inf)
  (var max-y math/-inf)
  (each c (if (indexed? y-columns) y-columns [y-columns])
    (set min-y (min min-y (extreme < (filter identity (get data c [0])))))
    (set max-y (max max-y (extreme > (filter identity (get data c [1]))))))

  [(or override-min-x min-x) (or override-max-x max-x)
   (or override-min-y min-y) (or override-max-y max-y)])

(defn- guess-axis-ticks
  "Given a set of numeric values, generate a reasonable array of tick marks given a minimum spacing.
  Biases the tick spacing to a power of 10 (or by 5s) for nicer charts by default. We need to know the labels and font
  used to draw the tick marks to avoid text overlapping itself. This is where the majority of the complexity comes from."
  [minimum maximum pixel-span min-spacing vertical font prefix suffix min-delta &opt force-formatter no-retry] # TODO - too many unnamed arguments
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
  # e.g. allow limiting to integers
  (when min-delta
    (set delta (max min-delta delta)))
  (def epsilon (* delta 0.001))

  # Get tick metrics
  (set metric (floorn delta metric))
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
  (def padding10 10)
  (each metric-coord result
    (def [x y] (text-measure (formatter metric-coord) font 1))
    (set max-text-width (max max-text-width x))
    (set max-text-height (max max-text-height y)))

  # Recalculate
  (def min-spacing :shadow (+ padding10 (if vertical max-text-height max-text-width)))

  # Retry if ticks are too close together
  (unless no-retry
    (if (> (+ padding10 min-spacing) delta)
      (break (guess-axis-ticks minimum maximum pixel-span min-spacing vertical font prefix suffix min-delta force-formatter true))))

  # TODO - use text boundaries to set padding
  [result formatter max-text-width max-text-height])

###
### API
###

(defn dark-mode
  ```
  Set dynamic color defaults to dark mode
  ```
  []
  (setdyn *background-color* g/black)
  (setdyn *grid-color* (g/rgb 0.3 0.3 0.3))
  (setdyn *stroke-color* g/white)
  (setdyn *text-color* g/white)
  nil)

(defn light-mode
  ```
  Set dynamic color defaults to light mode
  ```
  []
  (setdyn *background-color* g/white)
  (setdyn *grid-color* (g/rgb 0.8 0.8 0.8))
  (setdyn *stroke-color* g/black)
  (setdyn *text-color* g/black)
  nil)

(defn- color-lerp-internal
  [a b t]
  (def [ar ag ab aa] a)
  (def [br bg bb ba] b)
  (g/srgb
    (lerp ar br t)
    (lerp ag bg t)
    (lerp ab bb t)
    (lerp aa ba t)))

(defn color-lerp
  [a b t]
  "Linearly interpolate between 2 colors in RGB space. Colors are srgb encoded as 32 bit unsigned integers."
  (color-lerp-internal (g/as-srgb a) (g/as-srgb b) t))

(defn make-color-map
  "Create a function that linearly interpolates between colors for colormapping."
  [& colors]
  (def n-colors (length colors))
  (def n-1-colors (- n-colors 1))
  (def srgb-colors (map g/as-srgb colors))
  (fn :interp
    [t &]
    (def t :shadow (clamp t 0 1))
    (def a-index (math/floor (* t n-1-colors)))
    (def b-index (+ 1 a-index))
    (if (> b-index n-1-colors) (break (last colors)))
    (def t-at-a (/ a-index n-1-colors))
    (def t-at-b (/ b-index n-1-colors))
    (def ab-interval (- t-at-b t-at-a))
    (def u (clamp (/ (- t t-at-a) ab-interval) 0 1))
    # Sampling should not allocate.
    (color-lerp-internal (in srgb-colors b-index) (in srgb-colors a-index) u)))

(defn invert-color-map
  "Create an inverted color-map from an existing color map."
  [mapping]
  (fn :inverted-map [t] (mapping (- 1 t))))

(defn- color-hash-impl
  [x]
  (def rng (math/rng (hash x)))
  # Avoid blacks and whites
  (g/rgb (+ 0.2 (* 0.6 (math/rng-uniform rng)))
         (+ 0.2 (* 0.6 (math/rng-uniform rng)))
         (+ 0.2 (* 0.6 (math/rng-uniform rng)))))

(defn- color-hash
  "Given a value, generate a pseudo-random color for visualization based on parameter t in range [0, 1]"
  [t _x &opt color-seed]
  (default color-seed (dyn *color-seed*))
  # Clamp to integer so any floating point error across architecture or calculations doesn't
  # result in a totally different color. NOTE: since this relies on the "hash" function, it may
  # not be identical between Janet versions.
  (def input [(math/floor (* 0x10000000 (clamp t 0 1))) color-seed])
  (color-hash-impl input))

(defn- color-hash-label
  [_t x &opt color-seed]
  "Given a value, generate a pseudo-random color for visualization based on a label. This lets dynamic labels
  map to stable colors."
  (default color-seed (dyn *color-seed*))
  (color-hash-impl [x color-seed]))

(def color-maps
  ```
  A table containing various default color maps that can be used for rendering heat map data.
  Each value is a function mapping real numbers in the range [0, 1] to colors represented as 32 bit integers.
  ```
  @{:hash color-hash
    :hash-label color-hash-label
    :grayscale (make-color-map g/black g/white)
    :bluescale (make-color-map 0xFF330000 g/white)
    :redscale (make-color-map 0xFF000033 g/white)
    :greenscale (make-color-map 0xFF003300 g/white)
    :bluescale-black (make-color-map g/black g/blue)
    :redscale-black (make-color-map g/black g/red)
    :greenscale-black (make-color-map g/black g/green)
    :turbo
    (make-color-map
      0xFF3D1331 0xFF742B39 0xFFA34140 0xFFCA5845 0xFFE56D47 0xFFF88246
      0xFFFF9641 0xFFF7AC34 0xFFE8BF26 0xFFD2D21A 0xFFBDE018 0xFFA9EC23
      0xFF90F53A 0xFF74FA58 0xFF5AFE78 0xFF43FE97 0xFF38FAAD 0xFF34F1C3
      0xFF35E6D6 0xFF39D7E8 0xFF3AC7F4 0xFF36B4FC 0xFF2F9FFE 0xFF2587FC
      0xFF1A6FF7 0xFF1157EE 0xFF0A44E3 0xFF0533D4 0xFF0325C4 0xFF0118AE
      0xFF010E97 0xFF03047B)
    :magma
    (make-color-map
      0xFF030000 0xFF0F0202 0xFF1F0709 0xFF310C11 0xFF41101A 0xFF551125
      0xFF671032 0xFF720F3E 0xFF79104B 0xFF7E1558 0xFF7F1963 0xFF811F71
      0xFF81247E 0xFF812889 0xFF802C95 0xFF7E30A3 0xFF7B34AE 0xFF7738BB
      0xFF723DC8 0xFF6D42D3 0xFF674ADE 0xFF6154E8 0xFF5D60F0 0xFF5B6EF6
      0xFF5D7DF9 0xFF628AFB 0xFF6999FD 0xFF73A8FE 0xFF7CB5FE 0xFF88C4FE
      0xFF95D2FD 0xFFA1DFFD)
    :viridis
    (make-color-map
      0xFF16000D 0xFF1D000F 0xFF24010F 0xFF2D030F 0xFF34050F 0xFF39090E
      0xFF3D0C0D 0xFF41100B 0xFF441609 0xFF451B08 0xFF462107 0xFF462705
      0xFF462D05 0xFF463504 0xFF463B03 0xFF454403 0xFF444D02 0xFF415602
      0xFF3D5F02 0xFF396903 0xFF347505 0xFF2E7F09 0xFF288A0F 0xFF219318
      0xFF199D26 0xFF12A837 0xFF0CAF4B 0xFF07B665 0xFF03BD88 0xFF01C3AB
      0xFF01C9D3 0xFF03CDFA)})

(defn to-color-map
  "Map a keyword, function, array, or dictionary to a function that maps values to colors."
  [cmap]
  (cond
    (function? cmap) cmap
    (indexed? cmap) (make-color-map ;cmap)
    (keyword? cmap) (assert (get color-maps cmap) "unknown color map")
    (number? cmap) (fn [&] :constant-color-map cmap)
    (dictionary? cmap) (fn [_t x] :dictionary-color-map (get cmap x g/magenta))
    (errorf "unknown color map %v - expect function, array, tuple, table, struct, number, table, or keyword, got %v" cmap)))

###
### Argument groups
###

(defn draw-legend
  ```
  Draw a legend given a set of labels and colors

  `canvas` can be either nil to skip drawing or a gfx2d/image.

  * :background-color - the color of the background of the legend. Use :none to skip drawing a background.
  * :font - the font to use for legend text
  * :padding - the number of pixels to leave around all drawn content
  * :color-map - a table/struct that maps labels to colors
  * :legend-map - a table/struct that maps labels to text to draw
  * :line-color - color to draw frame border
  * :text-color - color of text
  * :labels - a list of labels to draw in the legend
  * :view-width - width of the enclosing view in pixels to help hint how to size the legend.
  * :frame - whether or not to draw a frame around the legend

  Return [w h] of the area that was or would be drawn if the legend were to be drawn.
  ```
  [canvas &named
   background-color font padding color-map labels view-width
   frame color-seed legend-map line-color text-color]
  (default font (dyn *font* default-font))
  (default padding (dyn *padding* default-padding))
  (default color-map :magma)
  (default legend-map {})
  (default view-width 0)
  (default background-color (dyn *background-color* default-background-color))
  (default line-color (dyn *grid-color* default-grid-color))
  (default text-color (dyn *text-color* default-text-color))
  (when canvas
    (def {:width width :height height} (g/unpack canvas))
    (when (and (not= :none background-color) frame) (g/fill-rect canvas 0 0 width height background-color)))
  (def label-height (let [[_ h] (text-measure "Mg" font 1)] h))
  (def swatch-size label-height)
  (def spacing (+ label-height padding 1))
  (def small-spacing (math/round (* 0.125 label-height)))
  # (def padding (if frame (+ padding 4) padding)) # add frame border
  (var y padding)
  (var x padding)
  (var max-x 0)
  (def cmap (to-color-map color-map))
  # multiply label index by this to get index as real 0-1.
  (def factor (let [len (length labels)] (if (<= len 1) 0.5 (/ (- len 1)))))
  (eachp [index i] labels
    (def lab (string (get legend-map i i)))
    (def [text-width _] (text-measure lab font 1))
    (def item-width (+ padding padding padding text-width swatch-size))
    (when (> (+ x item-width) view-width)
      (unless (= i (first labels)) (+= y spacing)) # don't skip first line
      (set x padding))
    (when canvas
      (def color (cmap (* factor index) i))
      (g/fill-rect canvas x y swatch-size swatch-size color)
      (text-draw canvas (+ x swatch-size padding) (+ small-spacing y) lab text-color font 1))
    (+= x (+ item-width padding))
    (set max-x (max max-x x)))
  (+= y (+ 1 padding))
  (when (and canvas frame)
    (def {:width width :height height} (g/unpack canvas))
    (draw-frame canvas 1 1 (- width 2) (- height 2) line-color 2)) # 2 pixel solid frame
  [max-x (+ label-height y)])

(defn- draw-color-map
  ```
  Draw a rectangle that describes a color-map. Will draw the gradient
  horizontally by default, but layout can be one of :h, :v, :horizontal, or :vertical.
  ```
  [canvas color-map x y w h &opt layout]
  (default layout :horizontal)
  (enum layout :horizontal :vertical :h :v)
  (def horiz (or (= layout :h) (= layout :horizontal)))
  (if horiz
    (for xx 0 w
      (def color (color-map (/ xx (- w 1))))
      (g/fill-rect canvas (+ x xx) y 1 h color))
    (for yy1 0 h
      (def yy (- h yy1 1)) # flip
      (def color (color-map (/ yy1 (- h 1))))
      (g/fill-rect canvas x (+ y yy) w 1 color)))
  canvas)

(defn draw-heat-legend
  ```
  Draw a legend that describes a heat-map color range.

  `canvas` can be either nil to skip drawing or a gfx2d/image.

  * :swatch-width - width of the color gradient in pixels
  * :swatch-height - height of the color gradient in pixels
  * :background-color - the color of the background of the legend
  * :font - the font to use for legend text
  * :padding - the number of pixels to leave around all drawn content
  * :color-map - a table/struct that maps labels to colors
  * :line-color - color to draw frame border
  * :text-color - color of text
  * :labels - a list of labels to draw in the legend
  * :frame - whether or not to draw a frame around the legend

  Return [w h] of the area that was or would be drawn if the legend were to be drawn.
  ```
  [canvas &named
   swatch-width swatch-height
   background-color font padding color-map
   frame line-color text-color layout labels]
  (default font (dyn *font* default-font))
  (default padding (dyn *padding* default-padding))
  (default background-color (dyn *background-color* default-background-color))
  (default line-color (dyn *grid-color* default-grid-color))
  (default text-color (dyn *text-color* default-text-color))
  (default labels [])

  (default layout :horizontal)
  (enum layout :horizontal :vertical :v :h)
  (def h (or (= layout :h) (= layout :horizontal)))
  (def font-scale 1)

  # Measure extra padding needed by labels
  (var [lw lh] [0 0])
  (each l labels
    (def [tw th] (text-measure l font font-scale))
    (set lw (max lw tw))
    (set lh (max lh th)))
  (def h-padding (+ padding (div lw 2)))
  (def v-padding (+ padding (div lh 2)))

  # Default length should be enough to fit all of the labels along the long axis
  (def default-len (math/ceil (max 256 (* (length labels) (+ 4 (max lw lh))))))
  (default swatch-width (if h default-len 64))
  (default swatch-height (if h 64 default-len))

  (when canvas
    (def {:width width :height height} (g/unpack canvas))
    (when (and (not= :none background-color) frame) (g/fill-rect canvas 0 0 width height background-color)))

  (when canvas
    (draw-color-map canvas (to-color-map color-map) (if h h-padding padding) (if h padding v-padding) swatch-width swatch-height layout))

  # Draw metric labels
  (when canvas
    (def llen (length labels))
    (for i 0 llen
      (def l (get labels i))
      (def t (/ i (dec llen)))
      (def [tw th] (text-measure l font font-scale))
      (if h
        (let [x (+ h-padding (math/floor (* t swatch-width)))]
          (text-draw canvas (- x (div tw 2)) (+ padding swatch-height padding) l text-color font font-scale 0))
        (let [y (+ v-padding (math/floor (* (- 1 t) swatch-height)))]
          (text-draw canvas (+ padding swatch-width padding) (- y (div th 2)) l text-color font font-scale 0)))))

  (when (and canvas frame)
    (def {:width width :height height} (g/unpack canvas))
    (draw-frame canvas 1 1 (- width 2) (- height 2) line-color 2)) # 2 pixel solid frame

  [(+ h-padding h-padding padding (if h (- padding) 0) swatch-width)
   (+ v-padding v-padding padding (if h 0 (- padding)) swatch-height)])

(defn draw-axes
  ```
  Draw the axis for the chart. Also return a function that can be used
  to convert a coordinate in the metric space to the screen space. Most parameters
  are optional with sane defaults, but canvas, x-min, x-max, y-min, y-max are all required.

  * :canvas - gfx2d/image to draw the axes on
  *   :width - (if no canvas provided) - make a new canvas with the given width in pixels
  *   :height - (if no canvas provided) - make a new canvas with the given height in pixels
  * :x-label - optional label for the x axis
  * :y-label - optional label for the y axis
  * :padding - the number of pixels to leave around all drawn content
  * :inner-padding - how many pixels to add between the axes frame and the internal graphing area. Defaults to 8.
  * :inner-padding-x - inner-padding for the x-axis only.
  * :inner-padding-y - inner-padding for the y-axis only.
  * :font - the font to use for axis text
  * :{x,y}-{min,max} - The bounds for coordinate system to draw
  * :grid - Style for drawing grid-lines. Can be nil (none), :none, :solid, or :stipple
  * :format-x - unary function (fn [x] ...) that returns a string to label x axis tick marks with
  * :format-y - unary function (fn [y] ...) that returns a string to label y axis tick marks with
  * :x-prefix - if format-x not provided, allows easily adding a string prefix to x axis tick mark labels
  * :y-prefix - if format-y not provided, allows easily adding a string prefix to y axis tick mark labels
  * :x-suffix - if format-x not provided, allows easily adding a string suffix to x axis tick mark labels
  * :y-suffix - if format-y not provided, allows easily adding a string suffix to y axis tick mark labels
  * :x-ticks - An array of x coordinates used marking the x axis rather than making a guess. These will be labeled.
  * :y-ticks - An array of y coordinates used marking the x axis rather than making a guess. These will be labeled.
  * :x-grid-ticks - An array of x coordinates that are used for drawing a grid. Can be combined with x-ticks for separate labels and grid-line locations.
  * :y-grid-ticks - An array of y coordinates that are used for drawing a grid. Can be combined with y-ticks for separate labels and grid-line locations.
  * :x-minor-ticks - How many minor tick marks, if any, to place between major tick marks on the x axis
  * :y-minor-ticks - How many minor tick marks, if any, to place between major tick marks on the y axis
  * :x-labels-vertical - Turn x labels vertical so more can fit on the axis
  * :min-x-spacing - When guessing x ticks, allow setting a lower limit to the metric spacing between ticks
  * :min-y-spacing - When guessing y ticks, allow setting a lower limit to the metric spacing between ticks
  * :tick-length - how many pixels long to make major tick marks (minor tick marks are 1/2 major tick marks)
  * :transpose - Consider the x axis to be the vertical axis instead of the horizontal axis
  * :grid-between-x - Put grid-lines between X-axis labels on the x-axis instead of on them.
  * :grid-between-y - Put grid-lines between X-axis labels on the y-axis instead of on them.

  Returns a 4-tuple [view:gfx2d/image to-pixel-space:fn to-metric-space:fn outer-canvas:gfx2d/image]

  * `view` is an image that can be used to draw inside the chart, clipped so you don't overwrite that axes.
  * `(to-pixel-space metric-x metric-y)` converts metric space coordinates to pixel space for plotting on `view`.
  * `(to-metric-space pixel-x pixel-y)` converts pixel coordinates to the metric space.
  * `outer-canvas` is the input canvas or newly create enclosing image for the entire figure.
  ```
  [&named canvas width height
   padding inner-padding inner-padding-x inner-padding-y font
   x-min x-max y-min y-max min-x-spacing min-y-spacing
   grid format-x format-y
   x-label y-label tick-length
   x-suffix x-prefix y-suffix y-prefix
   x-ticks y-ticks x-grid-ticks y-grid-ticks
   grid-between-x grid-between-y
   x-minor-ticks y-minor-ticks x-labels-vertical transpose]

  (def [canvas width height] :shadow (canvas-and-dimensions canvas width height))
  (default padding (dyn *padding* default-padding))
  (default font (dyn *font* default-font))
  (default grid :none)
  (assert canvas)
  (assert x-min)
  (assert x-max)
  (assert y-min)
  (assert y-max)

  # Check enums
  (enum grid :none :solid :stipple :fine-stipple)

  (def {:width width :height height} (g/unpack canvas))
  (assert (pos? width))
  (assert (pos? height))
  (def line-color (dyn *stroke-color* default-stroke-color))
  (def grid-color (dyn *grid-color* default-grid-color))

  (def orig-dx (- x-max x-min))
  (def orig-dy (- y-max y-min))
  (assert (pos? orig-dx))
  (assert (pos? orig-dy))
  (def font-height (let [[_ h] (text-measure "Mg" font 1)] h))
  (default inner-padding 8)
  (default inner-padding-x inner-padding)
  (default inner-padding-y inner-padding)
  (def font-half-height (div font-height 2))
  (default tick-length (div font-height 3))
  (def has-grid (not= grid :none))
  (def has-ticks (not has-grid))
  (def stipple-cycle (case grid :stipple 8 :fine-stipple 2 0))
  (def stipple-on (case grid :stipple 4 1))
  (def tick-height (if has-grid 10 (+ tick-length 3)))
  (def tick-trim (if has-grid 0 (- tick-height tick-length)))

  # Initial guess for x label width
  (def [first-guess-xticks _xformat x-labels-width x-labels-height]
    (if x-ticks
      (do
        (def fmt (if format-x format-x string))
        (var maxh 0)
        (each xt x-ticks
          (def [w h] (text-measure (fmt xt) font 1))
          (set maxh (max maxh (if x-labels-vertical w h))))
        [nil nil maxh maxh])
      (guess-axis-ticks x-min x-max width 20 x-labels-vertical font x-prefix x-suffix min-x-spacing format-x)))

  # Calculate top and bottom padding
  (def outer-top-padding (max padding font-half-height))
  (def outer-bottom-padding (+ padding font-height (if x-label (+ padding (if x-labels-vertical x-labels-width x-labels-height)) 0)))
  (def top-padding outer-top-padding)
  (def bottom-padding (+ outer-bottom-padding tick-height))

  # Draw X Label
  (when x-label
    (def [w _h] (text-measure x-label font 1))
    (def yy (- height padding font-height))
    (text-draw canvas (div (- width w) 2) yy x-label line-color font 1))

  # Guess y axis ticks - used to calculate left and right padding
  (def [yticks yformat y-axis-tick-label-width]
    (if y-ticks
      (do
        (def fmt (if format-y format-y string))
        (var maxw 0)
        (each yt y-ticks
          (def [w _h] (text-measure (fmt yt) font 1))
          (set maxw (max maxw w)))
        [y-ticks fmt maxw maxw])
      (guess-axis-ticks y-min y-max (- height top-padding bottom-padding) 20 true font y-prefix y-suffix min-y-spacing format-y)))

  # Calculate left and right padding once y-axis is guessed
  (def outer-left-padding (+ padding y-axis-tick-label-width (if y-label (+ padding font-height) 0)))
  (def outer-right-padding outer-left-padding) # make it symmetrical, looks much nicer
  (def left-padding (+ outer-left-padding tick-height))
  (def right-padding outer-right-padding)

  # Draw Y Label
  (when y-label
    (def [w _h] (text-measure y-label font 1))
    (text-draw canvas padding (div (+ height w top-padding (- bottom-padding)) 2) y-label line-color font 1 1))

  # Add buffers for min/max with grid-between-{x,y}.
  (def x-bound-pad # in pixel space
    (if grid-between-x
      (let [xt (or x-ticks [0])
            len (max 1 (length xt))]
        (/ (- (max-of xt) (min-of xt)) (* 2 len)))
      0))
  (def orig-dx :shadow (+ orig-dx x-bound-pad x-bound-pad))
  (def x-min :shadow (- x-min x-bound-pad))
  (def y-bound-pad # in pixel space
    (if grid-between-y
      (let [yt (or y-ticks [0])
            len (max 1 (length yt))]
        (/ (- (max-of yt) (min-of yt)) (* 2 len)))
      0))
  (def orig-dy :shadow (+ orig-dy y-bound-pad y-bound-pad))
  (def y-min :shadow (- y-min y-bound-pad))

  # Closure to convert metric space to pixel space - only can be done after full padding calculations
  (def scale-x (/ (- width left-padding right-padding inner-padding-x inner-padding-x) orig-dx))
  (def scale-y (- (/ (- height top-padding bottom-padding inner-padding-y inner-padding-y) orig-dy)))
  (def offset-x (- left-padding (- inner-padding-x) (* scale-x x-min)))
  (def offset-y (- height bottom-padding inner-padding-y (* scale-y y-min)))
  (defn convert
    [metric-x metric-y]
    [(+ offset-x (* scale-x metric-x))
     (+ offset-y (* scale-y metric-y))])

  # Create a cropped view inside our "Frame" that can then be used for rendering charts
  # Use this view when drawing gridlines inside charts to match rounding errors when rendering charts.
  (def frame-width (- width left-padding right-padding 1))
  (def frame-height (- height top-padding bottom-padding 1))
  (def view (g/viewport canvas
                        (+ 1 left-padding) (+ 1 top-padding)
                        frame-width frame-height))
  (def frame-scale-x (/ (- frame-width 1 inner-padding-x inner-padding-x) orig-dx))
  (def frame-scale-y (- (/ (- frame-height 1 inner-padding-y inner-padding-y) orig-dy)))
  (def frame-offset-x (- inner-padding-x (* frame-scale-x x-min)))
  (def frame-offset-y (- frame-height 1 inner-padding-y (* frame-scale-y y-min)))
  (defn view-convert
    [metric-x metric-y]
    [(+ frame-offset-x (* frame-scale-x metric-x))
     (+ frame-offset-y (* frame-scale-y metric-y))])
  (defn view-unconvert
    [pixel-x pixel-y]
    [(/ (- pixel-x frame-offset-x) frame-scale-x)
     (/ (- pixel-y frame-offset-y) frame-scale-y)])

  # Draw Y-axis labsl
  (assert yticks "unable to generate y ticks. Make your chart bigger?")
  (each metric-y yticks
    (def [_ pixel-y] (convert 0 metric-y))
    (def rounded-pixel-y (math/round pixel-y))
    (def text (yformat metric-y))
    (def [text-width] (text-measure text font 1))
    (text-draw canvas (- outer-left-padding text-width) (- rounded-pixel-y font-half-height) text line-color font 1))

  # Draw Y-axis grid lines and/or tick marks
  (default y-grid-ticks
    (if grid-between-y
      (seq [i :range [0 (dec (length yticks))]]
        (mean (slice yticks i (+ i 2))))
      yticks))
  (when has-ticks # in enclosing canvas
    (each metric-y y-grid-ticks
      (def [_ pixel-y] (convert 0 metric-y))
      (def rounded-pixel-y (math/round pixel-y))
      (g/plot canvas (+ tick-trim outer-left-padding) rounded-pixel-y (+ outer-left-padding tick-height) rounded-pixel-y grid-color)))
  (when has-grid # in chart view
    (each metric-y y-grid-ticks
      (def [_ pixel-y] (view-convert 0 metric-y))
      (def rounded-pixel-y (math/round pixel-y))
      (g/plot view 0 rounded-pixel-y frame-width rounded-pixel-y grid-color stipple-cycle stipple-on)))

  # Draw horizontal axis - allow manual override for x tick marks
  (def [xticks xformat]
    (if x-ticks [x-ticks (if format-x format-x string)]
      (guess-axis-ticks x-min x-max (- width left-padding right-padding) 20 x-labels-vertical font x-prefix x-suffix min-x-spacing format-x)))
  (assert xticks "unable to generate x ticks. Make your chart bigger?")
  (each metric-x xticks
    (def [pixel-x _] (convert metric-x 0))
    (def rounded-pixel-x (math/round pixel-x))
    (def text (xformat metric-x))
    (def [text-width text-height] (text-measure text font 1))
    (if x-labels-vertical
      (text-draw canvas (- rounded-pixel-x (* text-height 0.5)) (- height outer-bottom-padding (- text-width)) text line-color font 1 1)
      (text-draw canvas (- rounded-pixel-x (* text-width 0.5)) (- height outer-bottom-padding) text line-color font 1)))

  # Draw x-axis grid ticks or grid lines
  (default x-grid-ticks
    (if grid-between-x
      (seq [i :range [0 (dec (length xticks))]]
        (mean (slice xticks i (+ i 2))))
      xticks))
  (when has-ticks
    (each metric-x x-grid-ticks
      (def [pixel-x _] (convert metric-x 0))
      (def rounded-pixel-x (math/round pixel-x))
      (g/plot canvas rounded-pixel-x (- height outer-bottom-padding tick-trim) rounded-pixel-x (- height outer-bottom-padding tick-height) grid-color)))
  (when has-grid
    (each metric-x x-grid-ticks
      (def [pixel-x _] (view-convert metric-x 0))
      (def rounded-pixel-x (math/round pixel-x))
      (g/plot view rounded-pixel-x 0 rounded-pixel-x frame-height grid-color stipple-cycle stipple-on)))

  # Draw minor horizontal axis tick marks
  (when (and x-minor-ticks (< 1 (length x-grid-ticks)))
    (def len (length x-grid-ticks))
    (def dx-first (- (in x-grid-ticks 1) (in x-grid-ticks 0)))
    (def dx-last (- (in x-grid-ticks (- len 1)) (in x-grid-ticks (- len 2))))
    # we must draw minor ticks before and after the first and last major ticks until the edge of the axis
    (def padded-ticks [(- (in x-grid-ticks 0) dx-first) ;x-grid-ticks (+ (in x-grid-ticks (- len 1)) dx-last)])
    (loop [j :range [1 (length padded-ticks)]
           :let [i (- j 1)
                 t0x (in padded-ticks i)
                 t1x (in padded-ticks j)
                 dx (- t1x t0x)]
           xfloat :range [t0x (+ t1x 0.00001) (/ dx x-minor-ticks)]
           :let [x (math/round (first (convert xfloat 0)))]
           :when (and (> x left-padding) (< x (- width right-padding)))]
      (g/plot canvas x (- height outer-bottom-padding tick-height) x (- height outer-bottom-padding (div tick-height 2)) grid-color)))

  # Draw minor vertical axis tick marks
  (when (and y-minor-ticks (< 1 (length y-grid-ticks)))
    (def len (length y-grid-ticks))
    (def dy-first (- (in y-grid-ticks 1) (in y-grid-ticks 0)))
    (def dy-last (- (in y-grid-ticks (- len 1)) (in y-grid-ticks (- len 2))))
    # we must draw minor ticks before and after the first and last major ticks until the edge of the axis
    (def padded-ticks [(- (in y-grid-ticks 0) dy-first) ;y-grid-ticks (+ (in y-grid-ticks (- len 1)) dy-last)])
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

  [view
   (if transpose (fn :convert [x y] (view-convert y x)) view-convert)
   (if transpose (fn :unconvert [x y] (view-unconvert y x)) view-unconvert)
   canvas])

###
### Line Graphs
###

(defn- get-scatter-style
  "Get a stampable image to use for scatter plots. Either use an input image or generate one that looks nice."
  [ss color point-radius stroke-thickness]
  (when (= :gfx2d/image (type ss)) (break ss))
  (def point-radius :shadow (math/round point-radius))
  (def size (+ 1 (* 2 point-radius)))
  (def cd point-radius) # center x and center y
  (def img (g/blank size size 4))
  (def border (- (math/ceil (/ point-radius 4)) 1))
  # Should we use plotting functionality for small ticknesses?
  (cond
    (= ss :xplot)
    (do
      (def thick 0)
      (g/plot-path img [thick thick (- size thick 1) (- size thick 1)] color)
      (g/plot-path img [thick (- size thick 1) (- size thick 1) thick] color))
    (= ss :x)
    (do
      (def thick (math/floor (/ stroke-thickness 2)))
      (g/stroke-path img [thick thick (- size thick 1) (- size thick 1)] color thick)
      (g/stroke-path img [thick (- size thick 1) (- size thick 1) thick] color thick))
    (= ss :o)
    (do
      (def cxy (math/ceil (/ size 2)))
      (def eps -0.01)
      (g/ring img cxy cxy (- point-radius stroke-thickness eps) (- point-radius eps) color))
    (= ss :oplot)
    (do
      (def cxy (math/floor (/ size 2)))
      (g/plot-ring img cxy cxy point-radius color))
    (= ss :square)
    (g/fill-rect img 0 0 size size color)
    (= ss :diamond)
    (g/fill-path img [0 cd cd 0 (- size 1) cd cd (- size 1)] color)
    (errorf "unknown scatter style %v" ss))
  img)

(defn plot-line-graph
  ```
  Plot a line graph or scatter graph on a canvas. This function does not add a set of axis, title, or chart legend, it will only plot the graph lines and points from data.

  * :canvas - a gfx2d/image to draw on
  *   :width - (if no canvas provided) - make a new canvas with the given width in pixels
  *   :height - (if no canvas provided) - make a new canvas with the given height in pixels
  * :to-pixel-space - optional function (f x y) -> [pixel-x pixel-y]. Used to convert the metric space to pixel space when plotting points.
  * :data - a data frame to use for x and y data
  * :x-column - the name of the data frame column to use for the x axis
  * :y-column - a single column name or list of column names to use for the y coordinates and connected lines
  * :color-map - a dictionary mapping columns to colors. By default will hash column name to pseudo-random colors
  * :line-style - How to draw lines. Can be one of :stroke, :plot, :none, :bar, :area, or :stipple. Default is :plot.
  * :line-style-per-column - Optional dictionary to override line style by y-column name.
  * :circle-points - add circles around each point
  * :point-radius - how large to make the circles around each point in pixels
  * :super-sample - use super sampling to draw a larger image and then scale it down for anti-aliasing.
  * :bar-padding - space between bars in bar-charts
  * :stroke-thickness - thickness in pixels of the stroke of the graph when :line-type = :stroke
  * :x-colors - for bar and scatter plots, optionally set per-point/per-bar colors with an function (f x y index) called on each point.
  * :transpose - When transpose is enabled, draw bar and area charts from the y-axis instead of the x-axis (make horizontal bar charts). Should be used with a transposed axes.

  Returns the modified canvas image.
  ```
  [&named
   canvas width height
   data
   to-pixel-space
   line-style
   line-style-per-column
   x-column
   y-column
   circle-points
   point-radius
   x-colors
   bar-padding
   stroke-thickness
   super-sample
   color-map
   transpose]

  (def [canvas canvas-width canvas-height] :shadow (canvas-and-dimensions canvas width height))
  (default to-pixel-space (fn :convert [x y] [x y]))
  (default color-map :turbo)
  (default line-style-per-column {})
  (default line-style :plot)
  (default bar-padding 5)
  (default point-radius 3)
  (default stroke-thickness 1.5)
  (default super-sample 1)
  (def cmap (to-color-map color-map))

  # Super sampling!
  # Super sampling does not work well with pixel-based line styles, like :plot, :stipple
  # Intended for use with :stroke (and :bar, although usually not needed/wanted).
  (when (> super-sample 1)
    # TODO - support super sampling of very large images by breaking into tiles and redrawing multiple times. Yes, this would be slow.
    (def new-canvas (g/blank (* super-sample canvas-width) (* super-sample canvas-height)))
    (def temp-canvas (g/blank canvas-width canvas-height))
    (plot-line-graph :canvas new-canvas
                     :to-pixel-space (fn [x y] (def [x1 y1] (to-pixel-space x y)) [(* super-sample x1) (* super-sample y1)])
                     :data data
                     :x-column x-column
                     :y-column y-column
                     :circle-points circle-points
                     :bar-padding (* super-sample bar-padding)
                     :color-map cmap
                     :super-sample nil
                     :stroke-thickness (* super-sample stroke-thickness)
                     :point-radius (* super-sample point-radius)
                     :line-style-per-column line-style-per-column
                     :line-style line-style
                     :transpose transpose)
    # The resize + blend must match, as well as the destination pixels!
    # After resize, alpha is pre-multiplied
    (g/resize-into temp-canvas new-canvas true)
    (g/stamp-blend canvas temp-canvas :premul)
    (break canvas))

  # Allow single or multiple y-columns shorthand - draw first column on top
  (def y-columns (if (indexed? y-column) (reverse y-column) [y-column]))

  # Draw graph
  (def xs (get data x-column))
  (assert (indexed? xs))
  # multiply label index by this to get index as real 0-1.
  (def factor (let [len (length y-columns)] (if (<= len 1) 0.5 (/ (- len 1)))))
  (eachp [series-index ycol] y-columns
    (def graph-color (cmap (* series-index factor) ycol))
    (default x-colors (fn :default-x-colors [&] graph-color))
    (def ys (get data ycol))

    # Collect points - handle missing ys
    # Conversion to integers should be done with math/round later if needed, not math/floor!
    (def pts @[])
    (def metric-pts @[])
    (for i 0 (length xs)
      (def x (get xs i))
      (when x
        (def y (get ys i))
        (when y
          (array/push metric-pts x y)
          (def [x1 y1] (to-pixel-space x y))
          (array/push pts x1 y1))))

    # Keep these around to know x/y coordinates of multi-bar data points on screen.
    (def multi-bar-coords @{})

    # Plot lines between points
    (def line-style2 (get line-style-per-column ycol line-style))
    (enum line-style2 :plot :stipple :fine-stipple :stroke :bar :multi-bar :none :area)
    (def multi-bar (= line-style2 :multi-bar)) # multi-bar and bar share most of the same code
    (case (if multi-bar :bar line-style2)

      :stipple
      (do
        (def up-pts (array/slice pts))
        (loop [i :range [1 (length pts) 2]]
          (+= (up-pts i) 1))
        (g/plot-path canvas up-pts graph-color 8 5)
        (g/plot-path canvas pts graph-color 8 5))

      :fine-stipple
      (do
        (def up-pts (array/slice pts))
        (g/plot-path canvas pts graph-color 2 1))

      :plot
      (do
        (def up-pts (array/slice pts))
        (loop [i :range [1 (length pts) 2]]
          (+= (up-pts i) 1))
        (g/plot-path canvas pts graph-color)
        (g/plot-path canvas up-pts graph-color))

      :stroke
      (do
        (g/stroke-path canvas pts graph-color stroke-thickness))

      :area
      (do
        (if transpose
          (do
            (def min-y (get pts 1))
            (def max-y (last pts))
            (def left-x -1)
            (g/fill-path canvas [;pts left-x max-y left-x min-y] graph-color))
          (do
            (def min-x (first pts))
            (def max-x (get pts (- (length pts) 2)))
            (def bottom-y (inc canvas-height))
            (g/fill-path canvas [;pts max-x bottom-y min-x bottom-y] graph-color))))

      :bar
      (do
        (def [base-x base-y] (map math/round (to-pixel-space 0 0)))
        (def total-dx (math/abs (- (first pts) (get pts (- (length pts) 2)))))
        (def total-dy (math/abs (- (get pts 1) (last pts))))
        (def bar-spacing-x (/ total-dx (- (length pts) 2) 0.5))
        (def bar-spacing-y (/ total-dy (- (length pts) 2) 0.5))
        (def bar-width-x (- bar-spacing-x bar-padding)) # normal bar-chart
        (def bar-width-y (- bar-spacing-y bar-padding)) # transposed bar-chart
        (loop [i :range [0 (length pts) 2]]
          (def j (div i 2))
          (def x (get pts i))
          (def y (get pts (+ 1 i)))
          (def xm (get metric-pts i))
          (def ym (get metric-pts (+ 1 i)))
          (def xr (math/round x))
          (def yr (math/round y))
          (def color (x-colors (get xs j) (get ys j) j))
          # Find the gridlines between bars precisely for nice spacing.
          (def [x-pixel-gridline-after y-pixel-gridline-after]
            (if-let [xm-next (get metric-pts (+ i 2))]
              (to-pixel-space (mean [xm xm-next]) 0)
              [(+ x (/ bar-spacing-x 2)) (- y (/ bar-spacing-y 2))]))
          (def [x-pixel-gridline-before y-pixel-gridline-before]
            (if-let [xm-prev (get metric-pts (- i 2))]
              (to-pixel-space (mean [xm xm-prev]) 0)
              [(- x (/ bar-spacing-x 2)) (+ y (/ bar-spacing-y 2))]))
          (def n-sections (if multi-bar (length y-columns) 1))
          (if transpose
            (do
              (def y1 (math/round (+ y-pixel-gridline-after (math/floor (/ bar-padding 2)))))
              (def y2 (math/round (- y-pixel-gridline-before (math/ceil (/ bar-padding 2)))))
              (def w1 (math/round (+ y1 (/ (* series-index (- y2 y1)) n-sections))))
              (def w2 (math/round (+ y1 (/ (* (+ 1 series-index) (- y2 y1)) n-sections))))
              (put multi-bar-coords [series-index j] [xr (math/round (/ (+ w1 w2) 2))])
              (g/fill-rect canvas base-x (+ 1 w1) (- xr base-x) (- w2 w1 1) color))
            (do
              (def x1 (math/round (+ x-pixel-gridline-before (math/floor (/ bar-padding 2)))))
              (def x2 (math/round (- x-pixel-gridline-after (math/ceil (/ bar-padding 2)))))
              (def w1 (math/round (+ x1 (/ (* series-index (- x2 x1)) n-sections))))
              (def w2 (math/round (+ x1 (/ (* (+ 1 series-index) (- x2 x1)) n-sections))))
              (put multi-bar-coords [series-index j] [(math/round (/ (+ w1 w2) 2)) yr])
              (g/fill-rect canvas (+ 1 w1) base-y (- w2 w1 1) (- yr base-y) color)))))

      :none nil)

    # Plot points
    (when circle-points
      # Allow for different styles per column
      (def style1 (if (dictionary? circle-points) (get circle-points ycol true) circle-points))
      (def style (if (= true style1) :x style1))
      (def stamps @{})
      (loop [i :range [0 (length pts) 2]]
        (def x (get pts i))
        (def y (get pts (+ 1 i)))
        (def j (div i 2))
        (def color (x-colors (get xs j) (get ys j) j))
        (def stamp-key [style color point-radius stroke-thickness]) # memoize
        (def stamping-image
          (if-let [res (get stamps stamp-key)]
            res
            (set (stamps stamp-key) (get-scatter-style ;stamp-key))))
        (assert (= :gfx2d/image (type stamping-image)))
        (def {:width circle-w :height circle-h} (g/unpack stamping-image))
        # Multi-bar has different coordinates for drawing points.
        (def [px py] (get multi-bar-coords [series-index j] [x y]))
        (g/stamp-blend canvas stamping-image :premul
                       (math/round (- px -0.5 (* circle-w 0.5)))
                       (math/round (- py -0.5 (* circle-h 0.5)))))))

  canvas)

(defn line-chart
  ```
  Render a line chart. Returns a gfx2d/image which can be further manipulated with the spork/gfx2d module.

  Basic Parameters
  * :canvas - a gfx2d/image to draw on
  *   :width - (if no canvas provided) - make a new canvas with the given width in pixels
  *   :height - (if no canvas provided) - make a new canvas with the given height in pixels
  * :data - a data frame to use for data
  * :title - an optional title to add to the rendered image
  * :font - font used to draw text, including title, legend, and axes labels
  * :save-as - save the generated image to file. Can be any format supported by the gfx2d module
  * :x-column - the name of the data frame column to use for the x axis
  * :y-column - a single column or array of column names to use for the chart
  * :x-ticks - manually set the tick marks on the X axis instead of auto-detecting them
  * :y-ticks - manually set the tick marks on the Y axis instead of auto-detecting them

  Axes Styling
  * :inner-padding - the number of pixels of white space between x-min and the x-axes as well as y-min and the y-axes.
  * :inner-padding-x - inner-padding for x-axis only
  * :inner-padding-y - inner-padding for y-axis only
  * :x-label - optional label for the x axis
  * :y-label - optional label for the y axis
  * :grid - how to draw grid lines. One of :none, :solid, or :stipple
  * :x-suffix - add a string suffix to each tick label on the x-axis
  * :y-suffix - add a string suffix to each tick label on the x-axis
  * :x-prefix - add a string prefix to each tick label on the y-axis
  * :y-prefix - add a string prefix to each tick label on the y-axis
  * :x-ticks - Array of labeled x-coordinate locations.
  * :y-ticks - Array of labeled y-coordinate locations.
  * :x-grid-ticks - Array of x-coordinates for grid lines.
  * :y-grid-ticks - Array of y-coordinates for grid lines.
  * :x-minor-ticks - how many, if any, small ticks to add between each large tick mark on the x axis
  * :y-minor-ticks - how many, if any, small ticks to add between each large tick mark on the y axis
  * :x-labels-vertical - Turn x labels vertical so more can fit on the axis
  * :tick-length - how long to make major tick marks
  * :grid-between-x - Put grid-lines between X-axis labels on the x-axis instead of on them.
  * :grid-between-y - Put grid-lines between X-axis labels on the y-axis instead of on them.

  Chart Styling
  * :padding - the number of pixels of white space around various elements of the chart
  * :background-color - color of background, defaults to white. Use :none to skip drawing a background.
  * :text-color - color of text, defaults to black
  * :color-map - a dictionary mapping columns to colors. By default will hash column name to pseudo-random colors
  * :scatter - set to true to disable lines connecting points
  * :legend - set to true to add a legend to the top of the chart
  * :legend-map - a dictionary mapping column names to pretty text for the chart
  * :legend-padding - extra padding around legend area
  * :point-radius - radius of points when drawing a scatter plot
  * :line-style - How to draw lines. Can be one of :stroke, :plot, :none, :bar, :area, or :stipple. Default is :plot.
  * :line-style-per-column - Optional dictionary to override line style by y-column name.
  * :super-sample - Super Sample anti-aliasing for chart lines. Is a bit slow, but makes smooth plots. Works best with :stroke and :bar
  * :stroke-thickness - thickness in pixels of the stroke of the graph when :line-type = :stroke

  Axis Boundaries
  * :x-min - minimum x coordinate on chart
  * :x-max - maximum x coordinate on chart
  * :y-min - minimum y coordinate on chart
  * :y-max - maximum y coordinate on chart
  ```
  [&named
   canvas width height data
   font background-color text-color color-map
   point-radius
   x-min x-max y-min y-max
   padding inner-padding inner-padding-x inner-padding-y title
   circle-points
   scatter grid legend super-sample stroke-thickness
   format-x format-y
   save-as
   legend-map legend-padding
   tick-length
   line-style line-style-per-column bar-padding
   x-label y-label
   x-suffix x-prefix y-suffix y-prefix
   x-column y-column
   x-ticks y-ticks x-minor-ticks y-minor-ticks
   x-grid-ticks y-grid-ticks
   x-labels-vertical
   grid-between-x grid-between-y
   transpose]

  # Check parameters and set defaults.
  (assert data)
  (def skeys (sort (keys data)))
  (default x-column (first skeys))
  (default y-column (drop 1 skeys))
  (default padding (dyn *padding* default-padding))
  (default point-radius 3)
  (default color-map :turbo)
  (default background-color (dyn *background-color* default-background-color))
  (default text-color (dyn *text-color* default-text-color))
  (default font (dyn *font* default-font))
  (default circle-points false)
  (default grid :none)
  (default line-style :plot)
  (default legend :none)

  # Bar charts have some extra defaults to look better by default. Also allow for non-numeric x-coordinates.
  # All of these defaults can be overridden.
  (def bar-chart? (index-of line-style [:bar :multi-bar]))
  (def x-data (assert (in data x-column)))
  (def x-data-is-numbers? (all number? x-data))
  (def x-data-as-numbers (if x-data-is-numbers? x-data (range (length x-data))))
  (default grid-between-x (and bar-chart? (not transpose)))
  (default grid-between-y (and bar-chart? transpose))
  (default x-ticks (if (and (not x-data-is-numbers?) (not transpose)) x-data-as-numbers))
  (default y-ticks (if (and (not x-data-is-numbers?) transpose) x-data-as-numbers))
  (default format-x (if (and (not x-data-is-numbers?) (not transpose)) (fn [xi] (string (get x-data xi)))))
  (default format-y (if (and (not x-data-is-numbers?) transpose) (fn [yi] (string (get x-data yi)))))
  # Now shallow clone to replace x-data with x-data-as-numbers
  (def data :shadow (table ;(kvs data) x-column x-data-as-numbers))

  # Check enums
  (enum grid :none :solid :stipple :fine-stipple)
  (enum line-style :plot :stipple :fine-stipple :stroke :bar :multi-bar :none :area) # - allow for dictionary of styles
  (enum legend :none :top :top-left :top-right :bottom-left :bottom-right)

  # Allow variadic shorthand
  (def y-columns (if (indexed? y-column) y-column [y-column]))

  # Get canvas
  (def [canvas width height] :shadow (canvas-and-dimensions canvas width height))
  (when (not= :none background-color)
    (g/fill-rect canvas 0 0 width height background-color))

  # Render title section, and update view to cut out title
  (var title-padding 0)
  (when title
    (def title-scale 2)
    (def [title-width title-height] (text-measure title font title-scale))
    (set title-padding (+ padding title-height))
    (text-draw canvas (math/round (* 0.5 (- width title-width))) padding title text-color font title-scale))

  # Add legend if legend = :top. This makes a horizontal legend just below the title with no extra framing
  (default legend-padding (max 4 (div padding 4)))
  (when (= legend :top)
    (+= title-padding (div padding 2))
    (def view-width (- width padding padding))
    (def [lw lh] (draw-legend nil :font font :padding legend-padding :labels y-columns :legend-map legend-map :view-width view-width))
    (def legend-view (g/viewport canvas (math/floor (* (- width lw) 0.5)) title-padding lw lh true))
    (+= title-padding lh)
    (-= title-padding (math/floor (* 0.5 padding))) # just looks a bit better
    (draw-legend legend-view :font font :padding legend-padding :labels y-columns :color-map color-map
                 :legend-map legend-map :text-color text-color :view-width view-width))

  # Crop title section out of place where axis and charting will draw
  (def view (g/viewport canvas 0 title-padding width (- height title-padding)))

  # Draw axes
  (def [x-min x-max y-min y-max] :shadow
    (calculate-data-bounds data
                           (if transpose y-columns x-column)
                           (if transpose x-column y-columns)
                           x-min x-max y-min y-max))
  (def [graph-view to-pixel-space _to-metric-space]
    (draw-axes
      :canvas view
      :padding padding :inner-padding inner-padding
      :inner-padding-x inner-padding-x
      :inner-padding-y inner-padding-y
      :font font
      :grid grid
      :format-x format-x :format-y format-y
      :x-suffix x-suffix :x-prefix x-prefix
      :y-suffix y-suffix :y-prefix y-prefix
      :x-min x-min :x-max x-max
      :y-min y-min :y-max y-max
      :x-ticks x-ticks :y-ticks y-ticks :tick-length tick-length
      :x-grid-ticks x-grid-ticks :y-grid-ticks y-grid-ticks
      :x-label x-label :y-label y-label
      :x-minor-ticks x-minor-ticks
      :y-minor-ticks y-minor-ticks
      :x-labels-vertical x-labels-vertical
      :grid-between-x grid-between-x :grid-between-y grid-between-y
      :transpose transpose))

  # Render graph lines
  (plot-line-graph
    :canvas graph-view
    :to-pixel-space to-pixel-space
    :data data
    :x-column x-column
    :y-column y-columns
    :color-map color-map
    :line-style line-style
    :line-style-per-column line-style-per-column
    :super-sample super-sample
    :circle-points (or circle-points scatter)
    :stroke-thickness stroke-thickness
    :point-radius point-radius
    :bar-padding bar-padding
    :transpose transpose)

  # Draw internal legend if selected
  (when (index-of legend [:top-left :top-right :bottom-left :bottom-right])
    (def [lw lh] (draw-legend nil :font font :padding legend-padding :labels y-columns :legend-map legend-map :frame false))
    (def {:width gw :height gh} (g/unpack graph-view))
    (def legend-view
      (case legend
        :top-left (g/viewport graph-view padding padding lw lh true)
        :top-right (g/viewport graph-view (- gw lw padding) padding lw lh true)
        :bottom-left (g/viewport graph-view padding (- gh lh padding) lw lh true)
        :bottom-right (g/viewport graph-view (- gw lw padding) (- gh lh padding) lw lh true)))
    (when (not= :none background-color)
      (g/fill-rect legend-view 0 0 lw lh background-color))
    (draw-legend legend-view :font font :padding legend-padding :labels y-columns :view-width 0
                 :color-map color-map :legend-map legend-map :frame true))

  # Save to file
  (when save-as
    (g/save save-as canvas))

  canvas)

###
### Heat Maps
###
### Rather than using a "data-frame" abstraction, we just provide a way to
### provide a function that maps input row and column to a color or value. Such
### a function is usually a oneliner given most reasonable data structures.

(defn plot-heat-map
  ```
  Render a heat map on a set of axis. Will nicely fill the passed in image, so use a subview to draw to a section of the chart.

  Basic Parameters
  * :canvas - A gfx2d/image to draw on
  *   :width - (if no canvas provided) - make a new canvas with the given width in pixels
  *   :height - (if no canvas provided) - make a new canvas with the given height in pixels
  * :color-fn - Function `(color-fn x y)` that returns a gfx2d color (32 bit integer) used to color each cell in the heat-map. If color-fn evaluates to a falsey value, that cell will be left blank.
  * :cell-text-fn - Function `(cell-text-fn x y)` that returns an optional string to render for each cell.
  * :num-columns - Number of columns to draw.
  * :num-rows - Number of rows to draw.
  * :box-gap - Number of pixels between boxes on the heat map
  * :font - font used to draw optional text in cells
  * :cell-text-color - color of text, defaults to black or white depending on the cell color

  Returns the modified original canvas.
  ```
  [&named
   canvas width height
   color-fn
   cell-text-fn
   num-columns
   num-rows
   box-gap
   font
   cell-text-color]

  # Check parameters and set defaults.
  (assert num-columns)
  (assert num-rows)
  (assert color-fn)
  (def [canvas canvas-width canvas-height] :shadow (canvas-and-dimensions canvas width height))
  (default box-gap 0)
  (default font (dyn *font* default-font))

  # Calculate box sizes - not always integers!
  (def box-width (- (/ (- canvas-width box-gap) num-columns) box-gap))
  (def box-height (- (/ (- canvas-height box-gap) num-rows) box-gap))
  (loop [y :range [0 num-rows]
         x :range [0 num-columns]
         :let [color (color-fn x y)]
         :when color] # skip empty cells
    (def yflip (- num-rows 1 y))
    # Weird math to keep gap sizes consistent for a nice look when things don't divide perfectly.
    (def pixel-x (math/floor (+ box-gap (* x (+ box-gap box-width)))))
    (def pixel-y (math/floor (+ box-gap (* yflip (+ box-gap box-height)))))
    (def next-pixel-x (math/floor (* (+ 1 x) (+ box-gap box-width))))
    (def next-pixel-y (math/floor (* (+ 1 yflip) (+ box-gap box-height))))
    (g/fill-rect canvas pixel-x pixel-y (- next-pixel-x pixel-x) (- next-pixel-y pixel-y) color)

    # Per cell text
    (when-let [text (and cell-text-fn (cell-text-fn x y))]
      (def [w h] (text-measure text font 1 0))
      (def text-x (math/floor (- (mean [pixel-x next-pixel-x]) (/ w 2))))
      (def text-y (math/floor (- (mean [pixel-y next-pixel-y]) (/ h 2))))
      (def tcolor (or cell-text-color (if (< 0.6 (color-value color)) g/black g/white))) # black or white, maximizing contrast
      (text-draw canvas text-x text-y text tcolor font 1 0)))

  canvas)

(defn heat-map-chart
  ```
  Generate a heat map.

  Basic Parameters
  * :canvas - A gfx2d/image to draw on
  *   :width - (if no canvas provided) - make a new canvas with the given width in pixels
  *   :height - (if no canvas provided) - make a new canvas with the given height in pixels
  * :color-map - a color map keyword or function used to map numbers in the range [0, 1] to a color.
  * :save-as - optional path to save the chart

  Function Callback Input
  * :num-columns - Number of columns to draw.
  * :num-rows - Number of rows to draw.
  * :color-fn - Function `(color-fn x y)` that returns a gfx2d color used to color each cell in the heat-map. If color-fn evaluates to a falsey value, that cell will be left blank.
  * :cell-text-fn - Function `(cell-text-fn x y)` that returns an optional string to render for each cell. If the function evaluates to nil, no text will be drawn for that cell.

  Data Frame Input
  * :data - a dataframe table that contains a grid of cell
  * :data-scale - map numeric data to a [0.0, 1.0] range with a scale factor or function. Is the constant 1.0 by default.
  * :xs - a list of x columns - these are keys in `data`
  * :ys - (optional) keys into each column - by default this is just (range num-rows-in-data).

  Axes Styling
  * :x-ticks - manually set the tick marks on the X axis instead of auto-detecting them
  * :y-ticks - manually set the tick marks on the Y axis instead of auto-detecting them
  * :x-label - optional label for the x axis
  * :y-label - optional label for the y axis
  * :x-suffix - add a string suffix to each tick label on the x-axis
  * :y-suffix - add a string suffix to each tick label on the x-axis
  * :x-prefix - add a string prefix to each tick label on the y-axis
  * :y-prefix - add a string prefix to each tick label on the y-axis
  * :x-minor-ticks - how many, if any, small ticks to add between each large tick mark on the x axis
  * :y-minor-ticks - how many, if any, small ticks to add between each large tick mark on the y axis
  * :x-labels-vertical - Turn x labels vertical so more can fit on the axis
  * :tick-length - how long to make major tick marks

  Chart Styling
  * :box-gap - Number of pixels between boxes on the heat map. Default is 0.
  * :cell-font - font used to draw optional text in cells
  * :cell-text-color - color of text, defaults to black or white, depending on cell color
  * :font - font used to draw axes
  * :title-font - font used to draw title. Defaults to font.
  * :text-color - color of axes and title text
  * :padding - Number of pixels to separate various elements of the chart
  * :background-color - chart background color. Use :none to skip drawing a background.
  * :legend - one of :top, :bottom, :left, :right, :top-left, :top-right, :bottom-left, :bottom-right, or :none
  * :legend-labels - an array of evenly-spaced markers to put on the color map legend.
  * :legend-width - width of color map gradient in the legend in pixels
  * :legend-height - height of the color map gradient in the legend in pixels

  Returns a new canvas.
  ```
  [&named
   canvas width height
   data data-scale xs ys
   color-fn cell-text-fn
   num-columns num-rows
   font title-font cell-font
   color-map
   background-color
   text-color cell-text-color
   x-min x-max y-min y-max
   format-x format-y
   padding
   title
   box-gap
   legend legend-frame legend-labels
   x-label y-label
   x-suffix x-prefix y-suffix y-prefix
   x-ticks y-ticks x-minor-ticks y-minor-ticks tick-length
   x-labels-vertical
   legend-width legend-height
   save-as]

  # Check parameters and set defaults.
  (default padding (dyn *padding* default-padding))
  (default background-color (dyn *background-color* default-background-color))
  (default text-color (dyn *text-color* default-text-color))
  (default font (dyn *font* default-font))
  (default title-font font)
  (default legend :none)
  (default tick-length 0)

  # Allow a few ways to populate the heat-map with data
  (def color-map :shadow (to-color-map (or color-map :magma)))
  (default data [[]])
  (default xs (or (and num-columns (range num-columns)) (sort (keys data))))
  (default ys (range (or num-rows (length (get data (first xs))))))
  (default data-scale 1.0)
  (def scale-fn (if (function? data-scale) data-scale (fn [t] (* t data-scale))))
  (defn get-point [x y]
    (def xcol (get data (get xs x)))
    (scale-fn (get xcol y)))
  (default color-fn (fn [x y] (color-map (get-point x y))))
  (default format-x (if num-columns nil (fn [x] (string (get xs x)))))
  (def num-columns (length xs))
  (def num-rows (length ys))

  (enum legend :none :top :top-left :top-right :bottom-left :bottom-right :left :right :top :bottom)

  # Get canvas
  (def [canvas width height] :shadow (canvas-and-dimensions canvas width height))
  (when (not= background-color :none) (g/fill-rect canvas 0 0 width height background-color))

  # Render title section, and update view to cut out title
  (var title-padding 0)
  (when title
    (def title-scale 2)
    (def [title-width title-height] (text-measure title title-font title-scale))
    (set title-padding (+ padding title-height))
    (text-draw canvas (math/round (* 0.5 (- width title-width))) padding title text-color title-font title-scale))

  # Add legend on outside of chart
  (def legend-padding (max 4 (div padding 4)))
  (var [right-pad left-pad top-pad bottom-pad] [0 0 0 0])
  (when (index-of legend [:top :bottom :left :right])
    (default legend-frame false)
    (def layout (if (index-of legend [:top :bottom]) :h :v))
    (def [lw lh] (draw-heat-legend nil :font font :padding legend-padding :color-map color-map :labels legend-labels :layout layout
                                   :swatch-width legend-width :swatch-height legend-height :frame legend-frame))
    (def legend-view (g/viewport canvas
                                 (case legend
                                   :top (div (- width lw) 2)
                                   :bottom (div (- width lw) 2)
                                   :left padding
                                   :right (- width padding lw))
                                 (case legend
                                   :top title-padding
                                   :bottom (- height padding lh)
                                   :left (div (- height lh (div title-padding -2)) 2)
                                   :right (div (- height lh (div title-padding -2)) 2))
                                 lw lh true))
    (case legend
      :left (set left-pad (+ lw padding))
      :right (set right-pad (+ lw padding))
      :top (set top-pad (+ lh padding))
      :bottom (set bottom-pad (+ lh padding)))
    (draw-heat-legend legend-view :font font :padding legend-padding :color-map color-map :labels legend-labels :layout layout
                      :swatch-width legend-width :swatch-height legend-height :frame legend-frame
                      :text-color text-color))

  # Crop title section and legend padding out of place where axis and charting will draw
  (def view (g/viewport canvas
                        left-pad
                        (+ top-pad title-padding)
                        (- width right-pad left-pad)
                        (- height title-padding top-pad bottom-pad)))

  # Draw axes
  (def {:width view-width :height view-height} (g/unpack view))
  (default x-min -0.5)
  (default y-min -0.5)
  (default x-max (+ -0.5 num-columns))
  (default y-max (+ -0.5 num-rows))
  (def [graph-view to-pixel-space _to-metric-space]
    (draw-axes :canvas view
               :padding padding :inner-padding 0
               :font font
               :grid :none # grid doesn't work well with heat-map
               :min-x-spacing 1 :min-y-spacing 1
               :format-x format-x :format-y format-y
               :x-suffix x-suffix :x-prefix x-prefix
               :y-suffix y-suffix :y-prefix y-prefix
               :x-min x-min :x-max x-max
               :y-min y-min :y-max y-max
               :x-ticks x-ticks :y-ticks y-ticks :tick-length tick-length
               # TODO - enable this for nice grids
               # :x-grid-ticks x-grid-ticks :y-grid-ticks y-grid-ticks
               :x-label x-label :y-label y-label
               :x-minor-ticks x-minor-ticks
               :y-minor-ticks y-minor-ticks
               :x-labels-vertical x-labels-vertical))

  # Plot the heat-map
  (plot-heat-map
    :canvas graph-view
    :color-fn color-fn
    :cell-text-fn cell-text-fn
    :num-columns num-columns
    :num-rows num-rows
    :font cell-font
    :cell-text-color cell-text-color
    :box-gap box-gap)

  # Draw internal legend if selected
  (when (index-of legend [:top-left :top-right :bottom-left :bottom-right])
    (default legend-frame true)
    (def legend-layout :v)
    (def [lw lh] (draw-heat-legend nil :font font :padding legend-padding :color-map color-map :labels legend-labels
                                   :layout legend-layout :swatch-width legend-width :swatch-height legend-height :frame legend-frame))
    (def {:width gw :height gh} (g/unpack graph-view))
    (def legend-view
      (case legend
        :top-left (g/viewport graph-view padding padding lw lh true)
        :top-right (g/viewport graph-view (- gw lw padding) padding lw lh true)
        :bottom-left (g/viewport graph-view padding (- gh lh padding) lw lh true)
        :bottom-right (g/viewport graph-view (- gw lw padding) (- gh lh padding) lw lh true)))
    (when (not= :none background-color)
      (g/fill-rect legend-view 0 0 lw lh background-color))
    (draw-heat-legend legend-view :font font :padding legend-padding :color-map color-map
                      :swatch-width legend-width :swatch-height legend-height
                      :text-color text-color :labels legend-labels :layout legend-layout :frame legend-frame))

  # Save to file
  (when save-as
    (g/save save-as canvas))

  canvas)

###
### Packing chart (area chart) - show relative sizes of things by area.
###

(defn- second [xs] (get xs 1))
(defn- tab-to-df
  "convert a table/struct to a 2 column data-frame."
  [x]
  (def xs @[])
  (def ys @[])
  (eachp [k v] x (array/push xs k) (array/push ys v))
  @{:x xs :y ys})

(defn plot-packing-chart
  ```
  Draw a packing chart (relative area chart). Plot boxes for each value who sizes are proportianal to the value
  they represent. A more versatile and compact alternative to pie charts, especially when there are many categories.
  Returns either a new gfx2d/image or the passed-in :canvas.

  Basic Parameters:
  * :canvas - A gfx2d/image to draw on
  *   :width - (if no canvas provided) - make a new canvas with the given width in pixels
  *   :height - (if no canvas provided) - make a new canvas with the given height in pixels
  * :color-map - a color map keyword or function used to map numbers in the range [0, 1] to a color.

  Data Frame Input:
  * :data - a dataframe table that contains a grid of cell
  * :x-column - a column name to use a the category identifiers. Defaults to the first column.
  * :y-column - a column name to use for the area quantities. Defaults to the second column
  * :c-column - a column name to use for color grading. Defaults to the same as the y-column, but mapped to a range from 0 to 1.

  Data Table Input:
  * :data-map - A table or struct that maps keys as categories to values as proportional rectangle areas.

  Layout Parameters:
  * :omega - a number between 0 and 1 used to decide how to split rectangular areas. The default is 0.5
  * :sort-bins - If true, will sort bins from largest to smallest before layout. This usually results in better-looking charts. Default is true.
       For custom bin ordering before layout, use a dataframe input, set sort-bins to false, and order the rows as desired.

  Color and Theme:
  * :font - Font to use to draw text inside areas.
  * :no-text-resize - By default, text will be scaled to fill the space inside each area. Enabling this option keeps all text the same scale.
  * :text-color - Color to draw text inside areas. By default, will choose white or black to maximize contrast.
  * :padding - Number of pixels / 2 between areas.
  * :inner-padding - Minimum number of pixels between area text and the area border.
  * :background-color - Background color of canvas. Use :none to skip drawing a background.
  ```
  [&named
   canvas width height
   data-map
   data x-column y-column c-column
   padding inner-padding
   background-color
   text-color
   font color-map
   omega
   no-text-resize
   sort-bins]

  (def [canvas canvas-w canvas-h] :shadow (canvas-and-dimensions canvas width height))
  (def canvas (g/blank canvas-w canvas-h 4))
  (default padding 2)
  (default inner-padding 2)
  (default background-color (dyn *background-color* default-background-color))
  (default color-map :magma)
  (default font (dyn *font* default-font))
  (default sort-bins true)
  (def cmap (to-color-map color-map))

  # Normalize input data
  (default data (tab-to-df (assert data-map "need :data or :data-map argument")))
  (def skeys (sort (keys data)))
  (default x-column (first skeys))
  (default y-column (get skeys 1))
  (def xs (assert (get data x-column)))
  (def ys (assert (get data y-column)))

  # Allow a custom color column, but use the y-column by default with a reasonable color ramp.
  (def max-value (max-of ys))
  (def min-value (min-of ys))
  (def value-range (- max-value min-value))
  (def color-ramp-slope (cond c-column 1 (not= 0 value-range) (/ value-range) 0))
  (def color-ramp-offset (cond c-column 0 (not= 0 value-range) (- (/ min-value value-range)) 0.5))
  (default c-column y-column)
  (def cs (assert (get data c-column)))

  # Use zipped data for sorting
  (def zipped-data (map tuple xs ys cs))

  (var custom-draw nil)

  # Preprocess data
  (each y ys (assert (>= y 0) "cannot have area measurements less than 0"))
  (def total-area (sum ys))
  (if sort-bins
    (sort-by |(- (get $ 1)) zipped-data))

  (defn do-branch
    [x y w h categories]

    # Boxes are too small
    (if (<= w (+ padding padding)) (break))
    (if (<= h (+ padding padding)) (break))

    # Leaf cases
    (when (empty? categories) (break))
    (when (= 1 (length categories))
      (def [cat area colort] (first categories))
      (when custom-draw (break (custom-draw x y w h cat area colort)))
      (def text (string cat))
      (def t (+ color-ramp-offset (* color-ramp-slope colort)))
      (def color (cmap t cat))
      (def tcolor (or text-color (if (< 0.6 (color-value color)) g/black g/white))) # black or white, maximizing contrast
      (g/fill-rect canvas (+ padding x) (+ padding y) (- w padding padding 1) (- h padding padding 1) color)
      (def [tw th] (text-measure text font))
      (def wlimit (- w inner-padding inner-padding padding padding))
      (def hlimit (- h inner-padding inner-padding padding padding))
      (def noh (or (> th hlimit) (> tw wlimit)))
      (def nov (or (> tw hlimit) (> th wlimit)))
      (if (and noh nov) (break)) # TODO - other things besides omit label?
      (def orient (if noh 1 0))
      # Scale up text to fill rectangle
      (def tscale (if no-text-resize 1
                    (max 1 (math/floor (- (min (/ (case orient 0 w h) tw)
                                               (/ (case orient 0 h w) th))
                                          0.2)))))
      (def tw (* tw tscale))
      (def th (* th tscale))
      (text-draw canvas
                 (+ x (div (- w (case orient 0 tw th)) 2))
                 (+ y (div (- h (case orient 0 th (- tw))) 2))
                 text tcolor font tscale orient)
      (break))

    # Group categories for split
    (def split-dir (if (>= w h) :h :v))
    (def lhs @[])
    (def total-weight (sum (map second categories)))
    # TODO play with this parameter, make it a function of w, h, number of categories, etc.
    (default omega 0.5)
    (def split-weight (* omega total-weight))
    (var weight 0)
    # Don't check last to ensure a split
    (loop [ci :range [0 (- (length categories) 1)] :while (< weight split-weight)]
      (def c (get categories ci))
      (def [_ area] c)
      (array/push lhs c)
      (+= weight area))
    (def rhs (drop (length lhs) categories))
    (def fraction (/ weight total-weight))
    (def inv-fraction (- 1 fraction))

    # Split
    (def ish (= :h split-dir))
    (def left-x x)
    (def left-y y)
    (def left-w (math/round (* (if ish fraction 1) w)))
    (def left-h (math/round (* (if ish 1 fraction) h)))
    (def right-x (if ish (+ x left-w) x))
    (def right-y (if ish y (+ y left-h)))
    (def right-w (if ish (- w left-w) w))
    (def right-h (if ish h (- h left-h)))
    (do-branch right-x right-y right-w right-h rhs)
    (do-branch left-x left-y left-w left-h lhs))

  # Initial recursive call
  (when (not= :none background-color)
    (g/fill-rect canvas 0 0 canvas-w canvas-h background-color))
  (do-branch 0 0 canvas-w canvas-h zipped-data)

  canvas)
