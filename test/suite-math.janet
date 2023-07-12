(use ../spork/misc)
(use ../spork/test)
(use ../spork/math)

(start-suite)

(assert-docs "/spork/math")

(assert (= (extent (range 10)) [0 9])
        "extent")

(assert (= (extent (range 10)) (extent (reverse (range 10))))
        "extent rev")

(assert (= (sum-compensated []) 0)
        "sum sum-compensated ")

(assert (= (sum-compensated (range 10)) 45)
        "sum sum-compensated ")

(assert (= (sum-compensated [0.1 (/ 1 3)]) (+ 0.1 (/ 1 3)))
        "sum sum-compensated floats")

(assert (deep= @[2 1] (swap @[1 2] 0 1)))

(assert (do
          (def arr @[65 28 59 33 21 56 22 95 50 12 90 53 28 77 39])
          (quickselect arr 8)
          (deep= arr @[39 28 28 33 21 12 22 50 53 56 59 65 90 77 95]))
        "quickselect only k")

(assert
  (do
    (def arr
      (seq [i :down-to [1000 0]] i))
    (quickselect arr 300)
    (= (arr 300) 300))
  "quickselect long")

(assert
  (do
    (def arr
      (seq [i :down-to [1000 0]] i))
    (quickselect arr 500 10 620)
    (= (arr 300) 700))
  "quickselect long")


(def even [3 6 7 8 8 10 13 15 16 20])

(assert (= (quantile-sorted even 1) 20)
        "quantile sorted 1")

(assert (= (quantile-sorted even 0) 3)
        "quantile sorted 0")

(assert (= (quantile-sorted even 0.25) 7)
        "quantile sorted 0.3")

(assert (= (quantile-sorted even 0.5) 9)
        "quantile sorted 0.5")

(assert (= (quantile-sorted even 0.75) 15)
        "quantile sorted 0.4")

(def odd [3 6 7 8 8 9 10 13 15 16 20])

(assert (= (quantile-sorted odd 1) 20)
        "quantile sorted 1")
(assert (= (quantile-sorted odd 0) 3)
        "quantile sorted 0")

(assert (= (quantile-sorted odd 0.25) 7)
        "quantile sorted 0.3")

(assert (= (quantile-sorted odd 0.5) 9)
        "quantile sorted 0.5")

(assert (= (quantile-sorted odd 0.75) 15)
        "quantile sorted 0.4")

(assert (= (quantile [15 16 3 8 9 10 13 6 7 8 20] 0.5) 9)
        "quantile")

(assert (= (quantile-rank-sorted [1 2 3 4] 5) 1)
        "quantile rank sorted hi")

(assert (= (quantile-rank-sorted [1 2 3 4] 0) 0)
        "quantile rank sorted low")

(assert (= (quantile-rank-sorted [1 2 3 4] 3) 0.75)
        "quantile rank sorted")

(assert (= (quantile-rank [1 4 3 2] 5) 1)
        "quantile rank sorted hi")

(assert (= (quantile-rank [1 4 3 2] 0) 0)
        "quantile rank sorted low")

(assert (= (quantile-rank [1 4 3 2] 3) 0.75)
        "quantile rank sorted")

(assert (= (add-to-mean 14 5 53) 20.5)
        "add to mean")

(assert (= (mode [0 1 1 2 3 3 3 4]) 3)
        "mode")

(assert (= (median [8 9 10]) 9)
        "median")

(assert (= (median [1 2 3 4]) 2.5)
        "median avg")

(assert (zero? (relative-err 0 0))
        "relative err")

(assert (zero? (relative-err 14.5 14.5))
        "relative err")

(assert (= (relative-err 1 2) 0.5)
        "relative err")

(assert (approx-eq 1 1.00001)
        "approx equal")

(assert (approx-eq (harmonic-mean [2 3]) 2.4)
        "harmonic mean")

(assert (= (geometric-mean [2 8]) 4)
        "geometric mean")

(assert (= (geometric-mean [4 1 (/ 1 32)]) 0.5)
        "geometric mean")

(assert (= (root-mean-square [1 1]) 1)
        "root mean square 0")

(assert (approx-eq (root-mean-square [3 4 5]) 4.0828)
        "root mean square 1")

(assert (approx-eq (root-mean-square [-0.1 5 -2 10]) 5.679)
        "root mean square 2")

(assert (approx-eq (sample-skewness [0 1 1]) -1.732050808)
        "sample skewenes 0")

(assert (approx-eq (sample-skewness [2 4 6 3 1]) 0.5901286564)
        "sample skewenes 1")

(assert (approx-eq (sample-skewness [2 0 0]) 1.732050808)
        "sample skewenes 1")

(assert (zero? (sum-nth-power-deviations [0 0 0] 2))
        "sum nth power deviations 0")

(assert (= (sum-nth-power-deviations [0 1] 2) 0.5)
        "sum nth power deviations 1")

(assert (= (sum-nth-power-deviations [0 1] 3) 0)
        "sum nth power deviations 2")

(assert (= (sum-nth-power-deviations [0 1 2] 2) 2)
        "sum nth power deviations 3")

(assert (approx-eq (variance [1 2 3 4 5 6]) 2.91667)
        "variance 0")

(assert (= (variance [1]) 0)
        "variance 1")

(assert (= (sample-variance [1 2 3 4 5 6]) 3.5)
        "sample variance 0")

(assert (approx-eq (sample-variance [1 2 3 4 5 6 7 8 9 10]) 9.167)
        "sample variance 1")

(assert (approx-eq (sample-variance [1 1]) 0)
        "sample variance 2")

(assert (approx-eq (standard-deviation [2 4 4 4 5 5 7 9]) 2)
        "standard deviation 0")

(assert (approx-eq (standard-deviation [1 2 3]) 0.816497)
        "standard deviation 1")

(assert (approx-eq (standard-deviation [0 1 2 3 4 5 6 7 8 9 10]) 3.162)
        "standard deviation 2")

(assert (approx-eq (sample-standard-deviation [2 4 4 4 5 5 7 9]) 2.138)
        "sample standard deviation")

(assert (= (median-absolute-deviation [1 1 2 2 4 6 9]) 1)
        "median absolute deviation")

(assert (= (interquartile-range [3 6 7 8 8 10 13 15 16 20]) 8)
        "interquartile range")

(assert (= (z-score 78 80 5) -0.4) "z score")

(assert (approx-eq (sample-covariance [1 2 3 4 5 6] [6 5 4 3 2 1]) -3.5)
        "sample covariance")

(assert (approx-eq (sample-correlation [1 2 3 4 5 6] [2 2 3 4 5 60]) 0.690663)
        "sample correlation")

(assert (= (factorial 6) 720)
        "factorial")

(assert (= (linear-regression [[0 0] [1 1]]) {:m 1 :b 0})
        "linear regression")

(assert (= ((linear-regression-line
              (linear-regression [[0 0] [1 1]])) 1) 1)
        "linear regression line")

(assert (= (bernoulli-distribution 0.3) [0.7 0.3])
        "bernolli distribution")

(def bd [0.117649 0.302526 0.324135 0.18522 0.059535 0.010206 0.000729])
(var i 0)
(assert (not (empty? (binominal-distribution 6 0.3))))
(assert
  (all true?
       (seq [c :in (binominal-distribution 6 0.3) :after (++ i)]
         (approx-eq c (bd i))))
  "binominal distribution")

(def pd [0.0497871 0.149361 0.224042 0.224042 0.168031 0.100819 0.0504094
         0.021604 0.00810151 0.0027005 0.000810151 0.00022095])
(var i 0)
(assert (not (empty? (poisson-distribution 3))))
(assert
  (all true?
       (seq [c :in (poisson-distribution 3) :after (++ i)]
         (approx-eq c (pd i))))
  "poisson distribution")

(def tsnt
  [0.5 0.504 0.508 0.512 0.516 0.5199 0.5239 0.5279 0.5319 0.5359 0.5397
   0.5437 0.5477 0.5516 0.5556 0.5595 0.5635 0.5675 0.5714 0.5753 0.5793
   0.5832 0.587 0.5909 0.5948 0.5987 0.6026 0.6064 0.6102 0.614 0.6179
   0.6217 0.6254 0.6292 0.6331 0.6368 0.6405 0.6442 0.648 0.6516 0.6553
   0.6591 0.6627 0.6663 0.670 0.6735 0.6772 0.6807 0.6844 0.6878 0.6915
   0.6949 0.6985 0.7018 0.7054 0.7087 0.7123 0.7157 0.7189 0.7224 0.7257
   0.729 0.7324 0.7357 0.7389 0.7421 0.7453 0.7486 0.7517 0.7549 0.758
   0.7611 0.7641 0.7672 0.7703 0.7733 0.7763 0.7793 0.7823 0.7852 0.7881
   0.791 0.7939 0.7966 0.7994 0.8023 0.8051 0.8077 0.8105 0.8133 0.8158
   0.8185 0.8212 0.8237 0.8264 0.8288 0.8315 0.8339 0.8365 0.8388 0.8413
   0.8437 0.846 0.8485 0.8508 0.853 0.8554 0.8577 0.8599 0.862 0.8642 0.8665
   0.8686 0.8708 0.8729 0.8749 0.877 0.879 0.881 0.883 0.8849 0.8869 0.8888
   0.8907 0.8924 0.8943 0.8962 0.898 0.8997 0.9014 0.9032 0.9049 0.9065
   0.9082 0.9099 0.9114 0.9131 0.9146 0.9162 0.9176 0.9192 0.9206 0.9222
   0.9235 0.9251 0.9264 0.9278 0.9292 0.9305 0.9318 0.9332 0.9345 0.9356
   0.937 0.9382 0.9394 0.9405 0.9417 0.9428 0.9441 0.9452 0.9463 0.9474
   0.9484 0.9495 0.9505 0.9515 0.9525 0.9535 0.9545 0.9554 0.9564 0.9573
   0.9582 0.959 0.9598 0.9607 0.9616 0.9625 0.9633 0.964 0.9648 0.9656
   0.9664 0.967 0.9677 0.9686 0.9693 0.9698 0.9706 0.9713 0.9718 0.9726
   0.9731 0.9738 0.9744 0.9749 0.9756 0.976 0.9767 0.9771 0.9778 0.9782
   0.9788 0.9792 0.9798 0.9802 0.9808 0.9811 0.9817 0.982 0.9826 0.9829
   0.9834 0.9838 0.9841 0.9846 0.9849 0.9854 0.9857 0.986 0.9864 0.9868
   0.987 0.9875 0.9878 0.988 0.9883 0.9887 0.9889 0.9892 0.9896 0.9898
   0.99 0.9903 0.9906 0.9909 0.991 0.9912 0.9916 0.9918 0.9919 0.9921
   0.9925 0.9927 0.9929 0.993 0.9931 0.9933 0.9936 0.9938 0.9939 0.994
   0.9942 0.9945 0.9946 0.9948 0.9949 0.995 0.9951 0.9952 0.9955 0.9956
   0.9957 0.9959 0.996 0.996 0.9961 0.9962 0.9963 0.9965 0.9966 0.9967
   0.9968 0.9969 0.997 0.997 0.9971 0.9972 0.9973 0.9973 0.9975 0.9976
   0.9977 0.9977 0.9978 0.9979 0.9979 0.998 0.998 0.998 0.9981 0.9981
   0.9982 0.9982 0.9983 0.9983 0.9985 0.9985 0.9986 0.9986 0.9987 0.9987
   0.9988 0.9988 0.9988 0.9989 0.9989 0.9989 0.999])
(var i 0)
(assert
  (all true?
       (seq [c :in standard-normal-table
             :after (++ i)]
         (approx-eq c (tsnt i) 0.001))))

(assert (approx-eq (t-test [1 2 3 4 5 6] 3.385)
                   0.164942)
        "t-test")

(assert (approx-eq (t-test-2 [1 2 3 4] [3 4 5 6] 0)
                   -2.19089)
        "two sample t-test")

(assert (deep-not= (shuffle-in-place (range 10))
                   (range 10))
        "shuffle in place")

# this needs to be more precise I am affraid
(assert (= (permutation-test [2 2 2 2 2] [2 2 2 2 2])
           1)
        "permutation test same samples two side")

(assert (pos? (permutation-test [2 5 3 6 7 2 5] [20 5 13 12 7 2 2]))
        "permutation test two side")

(assert (pos? (permutation-test [2 5 3 6 7 2 5] [20 5 13 12 7 2 2]
                                :greater 1e3))
        "permutation test all args")

(assert chi-squared-distribution-table
        "chi squared distribution table")

(assert (approx-eq (cumulative-std-normal-probability 0.4) 0.6554)
        "cumulative std normal probability")

(assert (= (binominal-coeficient 6 4) 15)
        "binominal-coeficient")

(assert (= (binominal-coeficient 6 6) 1)
        "binominal-coeficient")

(assert (= (binominal-coeficient 6 7) 0)
        "binominal-coeficient")

(assert (deep= (permutations @[1 2]) @[@[1 2] @[2 1]])
        "permutations")

(assert (= (rows @[@[5 0 0] @[0 5 0]]) 2)
        "rows")

(assert (= (cols @[@[5 0 0] @[0 5 0]]) 3)
        "cols")

(assert (= (size @[@[5 0 0] @[0 5 0]]) [2 3])
        "size")

(assert (deep= (zero 2) @[0 0])
        "zero vector")

(assert (deep= (zero 2 2) @[@[0 0] @[0 0]])
        "zero matrix")

(let [m (zero 2 2)]
  (put-in m [0 0] 1)
  (assert (= ((m 1) 0) 0)
          "independent zero rows"))

(assert (deep= (ident 2) @[@[1 0] @[0 1]])
        "identity matrix")

(assert (deep= (ident 3) @[@[1 0 0] @[0 1 0] @[0 0 1]])
        "identity matrix")

(assert (deep= (scalar 3 5) @[@[5 0 0] @[0 5 0] @[0 0 5]])
        "scalar matrix")

(assert (deep= (trans @[@[1 2 3] @[4 5 6]])
               @[@[1 4] @[2 5] @[3 6]])
        "trans")

(assert (deep= (dot @[@[1 2] @[4 5]]
                    @[@[3 4] @[6 7]])
               @[@[15 18] @[42 51]])
        "dot")

(let [m @[@[1 2] @[4 5]]]
  (add m 3)
  (assert (deep= m @[@[4 5] @[7 8]])
          "add scalar"))


(let [m @[@[1 2] @[4 5]]]
  (mul m 3)
  (assert (deep= m @[@[3 6] @[12 15]])
          "mul scalar"))


(let [m @[@[1 2] @[4 5]]]
  (add m @[@[1 1] @[1 1]])
  (assert (deep= m @[@[2 3] @[5 6]])
          "add matrix"))

(assert (deep= (mul @[@[1 2 3] @[4 5 6]]
                    @[1 2 3]) @[@[14] @[32]])
        "mul vector")

(let [m @[@[10 20] @[30 40] @[50 60]]]
  (mul m @[@[1 2] @[3 4] @[5 6]])
  (assert (deep= m
                 @[@[10 40] @[90 160] @[250 360]])
          "mul matrix"))

(let [m @[@[1 2] @[4 5]]]
  (sop m * 3)
  (assert (deep= m @[@[3 6] @[12 15]])
          "scalar operation on matrix with argument"))

(let [m @[@[1 2] @[4 5]]]
  (sop m * 3 2)
  (assert (deep= m @[@[6 12] @[24 30]])
          "scalar operation on matrix with arguments"))

(let [m @[@[1 2] @[4 5]]]
  (sop m -)
  (assert (deep= m @[@[-1 -2] @[-4 -5]])
          "scalar operation on matrix without argument"))

(let [m @[@[10 20] @[30 40] @[50 60]]]
  (mop m * @[@[1 2] @[3 4] @[5 6]])
  (assert (deep= m
                 @[@[10 40] @[90 160] @[250 360]])
          "operation on matrix"))

(assert (= -2 (det @[@[1 2]
                     @[3 4]]))
        "determinant")

(assert (= 54 (det @[@[-2 -1 2]
                     @[2 1 4]
                     @[-3 3 -1]]))
        "determinant")

(assert (= 10 (perm @[@[1 2]
                      @[3 4]]))
        "pernament")

(assert (= -2 (perm @[@[-2 -1 2]
                      @[2 1 4]
                      @[-3 3 -1]]))
        "permanent")

(end-suite)
