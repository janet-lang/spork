###
### See available color maps with heat charts
###

(import spork/charts)
(import spork/gfx2d)

(def dfdata
  {:a (range 0 1 0.04)
   :b (range 0 1 0.03)
   :c (range 0 1 0.035)
   :d (range 0 2 0.029)
   :e (range 0.2 1.4 0.0391)})
(def font (gfx2d/load-font "examples/fonts/Roboto-Regular.ttf" 16))
(defn make-a-chart
  [color-map]
  (charts/heat-map-chart
    :width 500 :height 500
    :data dfdata
    :legend :right
    :legend-labels (seq [x :range [0 5]] (case x 0 "" 4 "" (string/format "%.2f" (/ x 4))))
    :font font
    :legend-width 20
    :title (string ":" color-map)
    :color-map color-map))

# Make a grid of charts
(def the-charts (map make-a-chart [:grayscale :turbo :viridis :magma]))
(def outer-canvas (gfx2d/blank 1000 1100 4))
(gfx2d/fill-rect outer-canvas 0 0 2000 2000 gfx2d/white)
(def title "spork/charts color maps")
(def [w h] (gfx2d/measure-text font title 3.0))
(gfx2d/draw-text outer-canvas font (- 500 (div w 2)) 20 title gfx2d/black 3.0)
(eachp [i chart] the-charts
  (def x (if (even? i) 0 500))
  (def y (if (> i 1) 100 600))
  (gfx2d/stamp outer-canvas chart x y))
(gfx2d/save "tmp/chart-grid.png" outer-canvas)
(print "Wrote to tmp/chart-grid.png")
