(import "/spork/cc" :as cc)
(import "/spork/sh" :as sh)

(defn install [m &]
  (bundle/add-file m "src/tarray.h" "tarray.h")
  (bundle/add m "spork")
  (when (dyn 'bundle/add-bin)
    (bundle/add-bin m "bin/janet-format")
    (bundle/add-bin m "bin/janet-netrepl"))
  (each file (os/dir "build")
    (def f (string "build/" file))
    (when (= (os/stat f :mode) :file)
      (bundle/add-file m f (string "spork/" file)))))

(defn clean [&]
  (sh/rm "build"))

(defn test [&]
  (var failure-count 0)
  (each suite (sorted (os/dir "test"))
    (when (string/has-prefix? "suite-" suite)
      (eprint "Running suite " suite)
      (def result (os/execute [(dyn *executable*) (string "test/" suite)] :p))
      (if-not (zero? result) (++ failure-count))))
  (if (zero? failure-count)
    (eprint "All Passed.")
    (do (eprintf "Failures: %v" failure-count) (os/exit 1))))

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

  (when (= :windows (os/which))
    (setdyn cc/*msvc-libs* @[(cc/msvc-janet-import-lib)]))

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
  (make1 "base64")
  (setdyn cc/*defines* {"_LARGEFILE64_SOURCE" true})
  (make1 "zip" "deps/miniz/miniz.c")

  (print "done!"))
