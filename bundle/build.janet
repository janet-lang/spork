(import "/spork/cc" :as cc)

(defn main
  [& argv]
  (os/mkdir "build")
  (os/mkdir "build/objects")
  (os/mkdir "build/spork")

  # How should we do toolchain detection?
  (def toolchain
    (cond
      (os/getenv "MSVC") :msvc
      (os/getenv "GCC") :gcc
      (os/getenv "CLANG") :clang
      (os/getenv "CC") :cc # any posix compatible compiler accessed via `cc`
      (= :windows (os/which)) :msvc
      :cc))

  (def suffix (case toolchain :msvc ".dll" ".so"))
  (def asuffix (case toolchain :msvc ".lib" ".a"))

  (def cc
    (case toolchain
      :msvc cc/msvc-compile-and-link-shared
      cc/compile-and-link-shared))

  (def compile-c
    (case toolchain
      :msvc cc/msvc-compile-c
      :msvc cc/compile-c))

  (def static-cc
    (case toolchain
      :msvc cc/msvc-compile-and-make-archive
      cc/compile-and-make-archive))

  (when (= :msvc toolchain)
    (cc/msvc-find)
    (assert (cc/msvc-setup?))
    (setdyn cc/*msvc-libs* @[(cc/msvc-janet-import-lib)]))

  (defn make1
    [name & other-srcs]
    (def from (string "src/" name ".c"))
    (def to (string "build/spork/" name suffix))
    (def toa (string "build/spork/" name asuffix))
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
