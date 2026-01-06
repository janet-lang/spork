(use spork/gfx2d)
(use spork/cjanet)

(import spork/gfx2d-shader :as s)

# Create checkerboard shader
(s/shader-begin
  :prefix "checker/"
  :shader-args '[color1:uint32_t color2:uint32_t])

(function shader
  [x:int y:int color1:uint32_t color2:uint32_t] -> uint32_t
  (return (? (band 1 (>> (bxor x y) 4)) color1 color2)))

(s/shader-end)

(defn rect
  [img x1 y1 x2 y2 color]
  (def xa (min x1 x2))
  (def xb (max x1 x2))
  (def ya (min y1 y2))
  (def yb (max y1 y2))
  (fill-path img [xa ya xa yb xb yb xb ya] color))

(defn line
  [img x1 y1 x2 y2 color]
  (stroke-path img [x1 y1 x2 y2] color 0.55))

# Blank canvas
(def img (blank 1024 1024 4))
(rect img 0 0 10000 10000 black)

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
(line img 12 12 12 1012 white)
(line img 12 1012 1012 1012 white)
(loop [x :range [112 1013 100]]
  (draw-simple-text img (- x 36) 995 1 1 (string (- x 12)) white :default)
  (line img x 1012 x (- 1012 30) white))
(loop [y :range [12 1011 100]]
  (def yy (- 1012 y))
  (draw-simple-text img 48 (- y 4) 1 1 (string yy) white :default)
  (line img 12 y (+ 12 30) y white))

# For line graph
'(stroke-path img path yellow)

(defn annotate
  [x &opt text]
  (default text (string/format "Y(%d)=%.2f" x (get ys x)))
  (def y (- 1012 (math/round (get ys x))))
  (line img (+ x 12) y (+ x 22) (- y 135) white)
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
