(use spork/gfx2d)

# Blank canvas
(def img (blank 1024 1024 4))
(rect img 0 0 10000 10000 0xFF111111)

# Generate some fake data
(def xs (range 1000))
(def ys (seq [x :in xs] (* 100 (+ (math/log (inc x)) (math/random)))))

# Draw axes
(line img 12 12 12 1012 white)
(line img 12 1012 1012 1012 white)
(loop [x :range [112 1013 100]]
  (draw-simple-text img (- x 36) 995 1 1 (string (- x 12)) white "default")
  (line img x 1012 x (- 1012 30) white))
(loop [y :range [12 1011 100]]
  (def yy (- 1012 y))
  (draw-simple-text img 48 (- y 4) 1 1 (string yy) white "default")
  (line img 12 y (+ 12 30) y white))

# Draw graph
(var prev-y nil)
(def step 5)
(loop [x :range [0 1000 step]]
  (def y (math/round (get ys x)))
  (when prev-y
    (line img (- (+ 12 x) step) (- 1012 prev-y) (+ 12 x) (- 1012 y) cyan))
  (set prev-y y))

(defn annotate
  [x text]
  (def y (- 1012 (math/round (get ys x))))
  (line img x y (+ x 22) (- y 135) white)
  (draw-simple-text img (+ x 30) (- y 150) 2 2 text white "olive"))

# Draw title
(draw-simple-text img 300 24 3 3 "Y = Log(X) + Noise" red "tall")

# Annotate the chart
(annotate 300 "Y(300)")
(annotate 500 "Y(500)")
(annotate 700 "Y(700)")

# Save it
(os/mkdir "tmp")
(save-png "tmp/chart.png" img)
(os/execute ["imv" "-u" "nearest_neighbour" "tmp/chart.png"] :px)

