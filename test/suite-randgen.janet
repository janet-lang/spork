(use ../spork/test)
(use ../spork/randgen)

(start-suite)

(assert-docs "/spork/randgen")

(def delta 0.00000000000001)

(assert (do
          (set-seed 1)
          (def expected @[0.205397787403126
                          0.946474426007273
                          0.292829399753968
                          0.579518994067899
                          0.897969128303416
                          0.218425627853671
                          0.583403751886178
                          0.613190880523174])
          (def actual
            (map |(do $ (rand-uniform))
                 (range (length expected))))
          (and (all |(> delta (math/abs (- $0 $1)))
                    expected actual)
               (all |(or (< 0 $ 1) (zero? $))
                    actual)))
        "rand-uniform")

(assert (do
          (set-seed 1)
          (def expected @[5.0318756480189 5.10069692026214 4.89567968233822 4.95051117259612])
          (def actual
           (sample-n |(rand-gaussian 5 0.1) 4))
          (and (all |(> delta (math/abs (- $0 $1)))
                    expected actual)))
        "sample-rand-gaussian")

(assert (do 
          (set-seed 1)
          (def expected 0.318756480188982) 
          (def actual (rand-gaussian)) 
          (|(> delta (math/abs (- $0 $1))) expected actual)
          "call-rand-gaussian"))

(assert (do
          (set-seed 1)
          (def low 0)
          (def hi 11)
          (def expected @[2 10 3 6 9 2 6 6])
          (def actual
            (map |(do $ (rand-int low hi))
                 (range (length expected))))
          (and (deep= expected actual)
               (all |(<= low $ (dec hi))
                    actual)))
        "rand-int")

(def animals [:ant :bee :cat :dog :ewe :fox :gnu :hog])

(assert (do
          (set-seed 1)
          (def expected @[1 7 2 4 7 1 4 4])
          (def actual
            (map |(do $ (rand-index animals))
                 (range (length expected))))
          (and (deep= expected actual)
               (all |(<= 0 $ (dec (length expected)))
                    actual)))
        "rand-index")

(assert (do
          (set-seed 1)
          (def expected @[:bee :hog :cat :ewe :hog :bee :ewe :ewe])
          (def actual
            (map |(do $ (rand-value animals))
                 (range (length expected))))
          (and (deep= expected actual)
               (all |(index-of $ animals)
                    actual)))
        "rand-value")

(assert (deep= (weights-to-cdf [2 6 8])
               @[0.125 0.5 1])
        "weights-to-cdf")

(def delta2 0.1)

(assert (do
          (set-seed 1)
          (def w1 1)
          (def w2 2)
          (def weights [w1 w2])
          (var n-w1-idx 0)
          (var n-w2-idx 0)
          (var other false)
          (loop [_ :range [0 1000]]
            (def res (rand-cdf (weights-to-cdf weights)))
            (cond
              (zero? res) (++ n-w1-idx)
              (one? res) (++ n-w2-idx)
              (set other true)))
          (and (not other)
               (> delta2 (math/abs (- (/ w2 w1) (/ n-w2-idx n-w1-idx))))))
        "rand-cdf")

(assert (do
          (set-seed 2)
          (def w1 1)
          (def w2 2)
          (def weights [w1 w2])
          (var n-w1-idx 0)
          (var n-w2-idx 0)
          (var other false)
          (loop [_ :range [0 10000]]
            (def res (rand-weights weights))
            (cond
              (zero? res) (++ n-w1-idx)
              (one? res) (++ n-w2-idx)
              (set other true)))
          (and (not other)
               (> delta2 (math/abs (- (/ w2 w1) (/ n-w2-idx n-w1-idx))))))
        "rand-weights")

(assert (do
          (set-seed 1)
          (def counts @{})
          (for _ 0 1000
            (rand-path (put counts :ant (inc (get counts :ant 0)))
                       (put counts :bee (inc (get counts :bee 0)))
                       (put counts :cat (inc (get counts :cat 0)))))
          (deep= counts @{:ant 344 :bee 318 :cat 338}))
        "rand-path")

(assert (do
          (set-seed 1)
          (def counts @{})
          (for _ 0 1000
            (rand-cdf-path (weights-to-cdf [1 2 3])
                           (put counts :ant (inc (get counts :ant 0)))
                           (put counts :bee (inc (get counts :bee 0)))
                           (put counts :cat (inc (get counts :cat 0)))))
          (deep= counts @{:ant 177 :bee 318 :cat 505}))
        "rand-cdf-path")

(assert (do
          (set-seed 1)
          (def counts @{})
          (for _ 0 1000
            (rand-weights-path [1 2 3]
                               (put counts :ant (inc (get counts :ant 0)))
                               (put counts :bee (inc (get counts :bee 0)))
                               (put counts :cat (inc (get counts :cat 0)))))
          (deep= counts @{:ant 177 :bee 318 :cat 505}))
        "rand-weights-path")

(end-suite)
