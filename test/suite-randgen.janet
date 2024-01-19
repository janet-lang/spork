(use ../spork/test)
(use ../spork/randgen)

(start-suite)

(assert-docs "/spork/randgen")

(def delta 0.0000000000001)

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
