(import "/spork/cc" :as cc)
(import "/spork/sh" :as sh)

(defn install [m &]
  (bundle/add-file m "src/tarray.h" "tarray.h")
  (bundle/add m "spork")
  (each file (os/dir "build")
    (def f (string "build/" file))
    (when (= (os/stat f :mode) :file)
      (bundle/add-file m f (string "spork/" file)))))

(defn clean [&]
  (sh/rm "build"))

(defn build [&]
  (os/mkdir "build")
  (os/mkdir "build/objects")

  (def suffix
    (case (os/which)
      :windows ".dll"
      ".so"))

  (def asuffix
    (case (os/which)
      :windows ".lib"
      ".a"))

  (def cc
    (case (os/which)
      :windows cc/msvc-compile-and-link-shared
      cc/compile-and-link-shared))

  (def static-cc
    (case (os/which)
      :windows cc/msvc-compile-and-make-archive
      cc/compile-and-make-archive))

  (defn make1
    [name & other-srcs]
    (def from (string "src/" name ".c"))
    (def to (string "build/" name suffix))
    (def toa (string "build/" name asuffix))
    (cc to from ;other-srcs)
    (static-cc toa from ;other-srcs))

  (setdyn cc/*visit* cc/visit-execute-if-stale)
  (setdyn cc/*build-dir* "build/objects")
  (make1 "crc")
  (make1 "json")
  (make1 "cmath")
  (make1 "utf8")
  (make1 "rawterm")
  (make1 "tarray")
  (setdyn cc/*defines* {"_LARGEFILE64_SOURCE" true})
  (make1 "zip" "deps/miniz/miniz.c")

  (print "done!"))
