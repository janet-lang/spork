(use spork/gfx2d)
(use spork/cjanet)

(import spork/gfx2d-shader :as s)

# Create a CJanet checkerboard pixel shader
# defines checker/fill-path, checker/stroke-path, etc.
(s/shader checker
  [color1:uint32_t color2:uint32_t]
  (function shader
    "Called on every pixel to paint a checkerboard pattern based on screen x,y coordinates"
    [x:int y:int color1:uint32_t color2:uint32_t] -> uint32_t
    (return (? (band 1 (>> (bxor x y) 2)) color1 color2))))

(defn line
  [img x1 y1 x2 y2 color]
  (stroke-path img [x1 y1 x2 y2] color 0.55))

# Blank canvas
(def img (blank 1024 1024 4))
(fill-rect img 0 0 10000 10000 black)

# Generate some fake data
(math/seedrandom 0)
(def xs (range 1000))
(def ys (seq [x :in xs] (* 100 (+ (math/log (inc x)) (math/random)))))

# Draw graph
(def xformed-xs (map (partial + 12) xs))
(def xformed-ys (map (partial - 1012) ys))
(def path (map math/round (mapcat tuple xformed-xs xformed-ys)))
#(printf "%.99M" (partition 8 path))
(checker/fill-path img [;path 1012 1000 12 1000] blue black)

# Draw axes
(plot img 12 12 12 1012 white)
(plot img 12 1012 1012 1012 white)
(loop [x :range [112 1013 100]]
  (draw-simple-text img (- x 36) 995 1 1 (string (- x 12)) white :default)
  (plot img x 1012 x (- 1012 30) white))
(loop [y :range [12 1011 100]]
  (def yy (- 1012 y))
  (draw-simple-text img 48 (- y 4) 1 1 (string yy) white :default)
  (plot img 12 y (+ 12 30) y white))

# For line graph
'(plot-path img path yellow)

(defn annotate
  [x &opt text]
  (default text (string/format "Y(%d)=%.2f" x (get ys x)))
  (def y (- 1012 (math/round (get ys x))))
  (plot img (+ x 12) y (+ x 22) (- y 135) white)
  (draw-simple-text img (+ x 30) (- y 150) 2 2 text white :olive))

# Draw title
(draw-simple-text img 300 24 3 3 "Y = Log(X) + Noise" red :tall)

# Annotate the chart
#(annotate 300 "Y(300)")
#(annotate 500 "Y(500)")
#(annotate 700 "Y(700)")
(annotate 118)

# Save it
(os/mkdir "tmp")
(print "writing tmp/chart.png")
(save-png "tmp/chart.png" img)
# (os/execute ["imv" "-u" "nearest_neighbour" "tmp/chart.png"] :px)
# Use feh, explorer.exe, whatever to view images
