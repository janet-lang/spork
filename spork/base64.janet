###
### base64.janet
###
### base64 encoder/decoder
###


(def- base64/table
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defn encode
  "Converts a string of any format (UTF-8, binary, ..) to base64 encoding."
  [input]
  (var cursor 0)
  (def rem (% (length input) 3))
  (def
    output
    (buffer/new-filled
      (->
        (length input)
        (+ rem)
        (div 3)
        (* 4))
      0))
  (each
    triplet
    (partition
      3
      (case rem
        0 input
        1 (buffer input @"\0\0")
        2 (buffer input @"\0")))
    (set
      (output cursor)
      (in base64/table (brshift (triplet 0) 2)))
    (set
      (output (+ cursor 1))
      (in
        base64/table
        (bor
          (-> (triplet 0) (band 2r11) (blshift 4))
          (brshift (triplet 1) 4))))
    (set
      (output (+ cursor 2))
      (in
        base64/table
        (bor
          (-> (triplet 1) (band 2r1111) (blshift 2))
          (brshift (triplet 2) 6))))
    (set
      (output (+ cursor 3))
      (in base64/table (band (triplet 2) 2r111111)))
    (set cursor (+ cursor 4)))
  (case rem
    1
    (do
      (set (output (- cursor 1)) 61)
      (set (output (- cursor 2)) 61))
    2 (set (output (- cursor 1)) 61))
  (string output))

(defn decode
  ```
  Converts a base64 encoded string to its binary representation of any format
  (UTF-8, binary, ..).
  ```
  [input]
  (def padded-input
    (case (% (length input) 4)
      0 input
      3 (string input "=")
      2 (string input "==")
      1 (error "Wrong length")))
  (def output (buffer/new-filled (* 3 (/ (length padded-input) 4)) 0))
  (var cursor 0)
  (each quadruple (partition 4 padded-input)
    (def values
      (map
        |(or
           (string/find (string/from-bytes $) base64/table)
           (if (= 61 $)
             0
             (errorf "Wrong character: %s" (string/from-bytes $))))
        quadruple))
    (set
      (output cursor)
      (bor
        (blshift (values 0) 2)
        (brshift (values 1) 4)))
    (set
      (output (+ cursor 1))
      (bor
        (blshift (values 1) 4)
        (brshift (values 2) 2)))
    (set
      (output (+ cursor 2))
      (bor
        (blshift (values 2) 6)
        (values 3)))
    (set cursor (+ cursor 3)))
  (slice
    output
    0
    (-
      (length output)
      (-
        (length padded-input)
        (length (string/trimr padded-input "="))))))
