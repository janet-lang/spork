(use spork/misc spork/test)
(use /spork/linalg)

(start-suite 1)

(assert-docs "/spork/linalg")

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
