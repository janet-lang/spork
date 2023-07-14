###
### base64.janet
###
### base64 encoder/decoder
###


(def- base64/table
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defn- array-pad-right
  [xs size padder]
  (let [l (length xs)]
    (if (< l size)
      (do (for i l size
            (put xs i padder))
        xs)
      xs)))

(defn- array-pad-left
  [xs size padder]
  (let [l (length xs)]
    (if (< l size)
      (do (for i 0 (- size l)
            (array/insert xs i padder))
        xs)
      xs)))

(defn- decimal->binary
  [x &opt bin]
  (default bin @[])
  (if (< x 1)
    (reverse bin)
    (let [rem (% x 2)
          new-x (math/floor (/ x 2))]
      (decimal->binary new-x (array/push bin rem)))))

(defn- binary->decimal
  [xs]
  (var num 0)
  (for i 0 (length xs)
    (if (= 1 (get (reverse xs) i))
      (set num (+ num (math/pow 2 i)))))
  num)

(defn- octets->sextets
  [octets]
  (->> octets
       flatten
       (partition 6)
       (map |(array ;$0))))

(defn- sextets->octets
  [octets]
  (->> octets
       flatten
       (partition 8)))

(defn- quadruples->bytes [xs]
  (let [sextets (map (fn [x]
                       (-> (string/find (string/from-bytes x) base64/table)
                           (decimal->binary)
                           (array-pad-left 6 0))) xs)
        octets (sextets->octets sextets)]
    (apply string/from-bytes (map binary->decimal octets))))

(defn- pad-last-sextet [xs]
  (let [last-index (dec (length xs))]
    (update xs last-index array-pad-right 6 0)))

(defn- add-padding [s]
  (if (zero? (% (length s) 4))
    s
    (let [pad-count (- 4 (% (length s) 4))]
      (string s (string/repeat "=" pad-count)))))

(defn encode
  "Converts a string of any format (UTF-8, binary, ..) to base64 encoding."
  [s]
  (if (> (length s) 0)
    (let [octets (map |(-> $0
                           decimal->binary
                           (array-pad-left 8 0))
                      (string/bytes s))
          sextets (pad-last-sextet (octets->sextets octets))
          bytes (map binary->decimal sextets)
          base64-bytes (map (fn [i] (get base64/table i)) bytes)
          base64 (add-padding (apply string/from-bytes base64-bytes))]
      base64)
    ""))

(defn decode
  ```
  Converts a base64 encoded string to its binary representation of any format
  (UTF-8, binary, ..).
  ```
  [s]
  (if-not (empty? s)
    (let [without-padding (string/replace-all "=" "" s)
          padded? (not (zero? (% (length without-padding) 4)))
          quadruples (partition 4 without-padding)
          bytes (map quadruples->bytes quadruples)
          base64 (apply string bytes)]
      (if padded? (slice base64 0 (dec (length base64))) base64))
    ""))
