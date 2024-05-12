(import "/spork/cc" :as cc)

(os/mkdir "build")
(os/mkdir "build/objects")

(def suffix
  (case (os/which)
    :windows ".dll"
    ".so"))

(def cc
  (case (os/which)
    :windows cc/msvc-compile-and-link-shared
    cc/compile-and-link-shared))

(defn make1
  [name & other-srcs]
  (def from (string "src/" name ".c"))
  (def to (string "build/" name suffix))
  (cc to from ;other-srcs))

(with-dyns [cc/*visit* cc/visit-execute
            cc/*build-dir* "build/objects"]
  (make1 "crc")
  (make1 "json")
  (make1 "cmath")
  (make1 "utf8")
  (make1 "rawterm")
  (make1 "tarray")
  (with-dyns [cc/*defines* {"_LARGEFILE64_SOURCE" true}]
    (make1 "zip" "deps/miniz/miniz.c")))
