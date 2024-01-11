(use ../spork/test)
(use ../spork/randgen)

(start-suite)

(assert-docs "/spork/randgen")

(def delta 0.000000000000009)

(assert (do
          (set-seed 1)
          (def expected @[5.00917440069385 
                          5.0318756480189 
                          4.97222830484943
                          5.10069692026214])
          (def actual
            (sample-n |(rand-gaussian 5 0.1) 4))
          (and (all |(> delta (math/abs (- $0 $1)))
                    expected actual)))
        "sample-rand-gaussian")
