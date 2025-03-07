###
### declare-cc.janet
###
### Declarative interface for spork/cc and spork/build-rules.
###
### Define builds with (declare-native ...) and (declare-executable ...)
### instead of sequential build recipes. This should also serve as a shorthand for cross platform builds, and
### is less flexible than raw spork/cc.
###

### TODO
# declare-project
# declare-binscript
# declare-bin
# declare-archive
# declare-executable
# declare-native
# custom build dir

(import ./build-rules :as build-rules)
(import ./cc :as cc)

(defdyn *toolchain* "Force a given toolchain. If unset, will auto-detect.")

(defn- get-toolchain
  "Auto-detect the current compiler toolchain."
  []
  (def toolchain
    (cond
      (dyn *toolchain*) (dyn *toolchain*)
      (os/getenv "MSVC") :msvc
      (os/getenv "GCC") :gcc
      (os/getenv "CLANG") :clang
      (os/getenv "CC") :cc # any posix compatible compiler accessed via `cc`
      (= :windows (os/which)) :msvc
      :cc))
  (when (= :msvc toolchain)
    (cc/msvc-find)
    (assert (cc/msvc-setup?))
    (setdyn cc/*msvc-libs* @[(cc/msvc-janet-import-lib)]))
  toolchain)

(def- entry-replacer
  "Convert url with potential bad characters into an entry-name"
  (peg/compile ~(% (any (+ '(range "AZ" "az" "09" "__") (/ '1 ,|(string "_" ($ 0) "_")))))))

(defn entry-replace
  "Escape special characters in the entry-name"
  [name]
  (get (peg/match entry-replacer name) 0))

(defn embed-name
  "Rename a janet symbol for embedding."
  [path]
  (->> path
       (string/replace-all "\\" "___")
       (string/replace-all "/" "___")
       (string/replace-all ".janet" "")))

(defn entry-name
  "Name of symbol that enters static compilation of a module."
  [name]
  (string "janet_module_entry_" (entry-replace name)))

(defn create-buffer-c
  ``Inline raw byte file as a c file. The header file will contain two exported symbols, `(string name "_emded")`, a
    pointer of an array of unsigned char, and `(string name "_embed_size")`, a size_t of the number bytes.``
  [bytes dest name]
  (def chunks (seq [b :in bytes] (string b)))
  (def lasti (- (length bytes) 1))
  (with [out (file/open dest :wn)]
    (file/write out "#include <stddef.h>\n\nstatic const unsigned char bytes[] = {")
    (def buf @"")
    (eachp [i b] bytes
      (if (= i lasti)
        (buffer/push buf (string b))
        (buffer/push buf (string b) ", "))
      (when (> 64000 (length buf)) # Don't run out of memory
        (file/write out buf)
        (buffer/clear buf)))
    (file/write out "};\n\n"
              "const unsigned char * const " name "_embed = bytes;\n"
              "const size_t " name "_embed_size = sizeof(bytes);\n")))

(defn declare-native
  "Declare a native module. This is a shared library that can be loaded
  dynamically by a janet runtime. This also builds a static libary that
  can be used to bundle janet code and native into a single executable."
  [&named name source embedded lflags libs cflags
   c++flags defines install nostatic static-libs
   use-rpath use-rdynamic pkg-config-flags
   pkg-config-libs smart-libs c-std c++-std target-os]

  (def rules (curenv)) # TODO

  # Defaults
  (default embedded @[])
  (default nostatic false)
  (default defines @{})

  # Create build environment table
  (os/mkdir "build") # TODO
  (def benv @{cc/*build-dir* "build"
              cc/*defines* defines
              cc/*libs* libs
              cc/*lflags* lflags
              cc/*cflags* cflags
              cc/*c++flags* c++flags
              cc/*static-libs* static-libs
              cc/*smart-libs* smart-libs
              cc/*use-rdynamic* use-rdynamic
              cc/*use-rpath* use-rpath
              cc/*pkg-config-flags* pkg-config-flags
              cc/*c-std* c-std
              cc/*c++-std* c++-std
              cc/*target-os* target-os
              # TODO
              cc/*visit* cc/visit-add-rule
              cc/*rules* rules
              })
  (table/setproto benv (curenv)) # configurable?
  # TODO - wrapper for cc/search-libraries, etc.
  (when pkg-config-libs (with-env benv (cc/pkg-config ;pkg-config-libs))) # Add package config flags

  (def toolchain (get-toolchain))
  (def suffix (case toolchain :msvc ".dll" ".so"))
  (def asuffix (case toolchain :msvc ".lib" ".a"))

  (def compile-c
    (case toolchain
      :msvc cc/msvc-compile-c
      cc/compile-c))

  (def cc
    (case toolchain
      :msvc cc/msvc-compile-and-link-shared
      cc/compile-and-link-shared))

  (def static-cc
    (case toolchain
      :msvc cc/msvc-compile-and-make-archive
      cc/compile-and-make-archive))

  (def to (string "build/" name suffix))
  (def toa (string "build/" name asuffix))

  # TODO - static library metadata

  (with-env benv
    (def objects @[])
    (loop [src :in embedded]
      (def c-src (cc/out-path src ".c"))
      (def o-src (cc/out-path src ".c.o"))
      (array/push objects o-src)
      (build-rules/build-rule rules c-src [src]
                              (create-buffer-c (slurp src) c-src (embed-name src)))
      (compile-c c-src o-src))
    (cc to ;source ;objects)
    (build-rules/build-rule rules :build [to])
    (unless nostatic
      (with-dyns [cc/*build-dir* "build/static"]
        (def sobjects @[])
        (loop [src :in embedded]
          (def c-src (cc/out-path src ".c"))
          (def o-src (cc/out-path src ".c.o"))
          (array/push sobjects o-src)
          (build-rules/build-rule rules c-src [src]
                                  (create-buffer-c (slurp src) c-src (embed-name src)))
          (compile-c c-src o-src))
        (static-cc toa ;source ;sobjects)
        (build-rules/build-rule rules :build [toa]))))

  rules)
