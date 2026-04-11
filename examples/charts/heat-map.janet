###
### Heat maps in 2 ways - closures vs. dataframes
###

(import spork/charts)
(import spork/gfx2d)

###
### The closure-driven way
###
(def cmap (get charts/color-maps :turbo))
(defn distfrom [px py] (fn [x y] (let [dx (- px x) dy (- py y)] (math/sqrt (+ (* dx dx) (* dy dy))))))
(def d1 (distfrom 10 10))
(def d2 (distfrom 20 18))
(def d3 (distfrom 37 8))
(charts/heat-map-chart
  :width (* 1 1920) :height (* 1 1080)
  :num-columns (* 1 40) :num-rows (* 1 25)
  :cell-text-fn (fn [x y] (string/format "%d,%d" x y))
  :color-fn (fn [x y]
              (def t (min (* 0.1 (d1 x y)) (* 0.2 (d2 x y)) (* 0.03 (d3 x y))))
              (cmap (+ (* 0.01 (math/random)) t)))
  :box-gap 2
  :cell-font :olive
  :font (gfx2d/load-font "examples/fonts/Roboto-Regular.ttf" 24)
  :title "Heat Map Example"
  :save-as "tmp/heat-map.png")
(print "Made heat map at tmp/heat-map.png")

###
### The data-driven way
###
(def dfdata
  {:a (range 0 1 0.04)
   :b (range 0 1 0.03)
   :c (range 0 1 0.035)
   :d (range 0 2 0.029)
   :e (range 0.2 1.4 0.0391)})
(charts/dark-mode)
(charts/heat-map-chart
  :width 700 :height 700
  :data dfdata
  :legend :right
  :legend-labels (seq [x :range [0 5]] (case x 0 "" 4 "" (string/format "%.2f" (/ x 4))))
  :font (gfx2d/load-font "examples/fonts/Roboto-Regular.ttf" 24)
  :title "Heat Map"
  :color-map :viridis
  :save-as "tmp/heat-map-dataframe.png")
(print "Made heat map at tmp/heat-map-dataframe.png")
