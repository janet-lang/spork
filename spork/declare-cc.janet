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
# declare-executable

(import ./build-rules)
(import ./cc)
(import ./path)
(import ./sh)

(defdyn *install-manifest* "Bound to the bundle manifest during a bundle/install.")
(defdyn *toolchain* "Force a given toolchain. If unset, will auto-detect.")

(defn- build-dir [] (dyn cc/*build-dir* "build"))
(defn- get-rules [] (dyn cc/*rules* (curenv)))

(defn- is-win-or-mingw
  []
  (def tos (dyn cc/*target-os* (os/which)))
  (or (= :windows tos) (= :mingw tos)))

(def- colors
  {:green "\e[32m"
   :red "\e[31m"})

(defn- color
  "Color text with ascii escape sequences if (os/isatty)"
  [input-color text]
  (if (os/isatty)
    (string (get colors input-color "\e[0m") text "\e[0m")
    text))

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
      (os/compiler)))
  (when (= :msvc toolchain)
    (cc/msvc-find)
    (assert (cc/msvc-setup?))
    (setdyn cc/*msvc-libs* @[(cc/msvc-janet-import-lib)]))
  toolchain)

(defn- install-rule
  [src dest]
  (def rules (get-rules))
  (build-rules/build-rule
    rules :install [src]
    (def manifest (assert (dyn *install-manifest*)))
    (bundle/add manifest src dest))
  nil)

(defn- install-buffer
  [contents dest &opt chmod-mode]
  (def rules (get-rules))
  (def contents (if (function? contents) (contents) contents))
  (build-rules/build-rule
    rules :install []
    (def manifest (assert (dyn *install-manifest*)))
    (def files (get manifest :files @[]))
    (put manifest :files files)
    (def absdest (path/join (dyn *syspath*) dest))
    (when (os/stat absdest :mode)
      (errorf "collision at %s, file already exists" absdest))
    (spit absdest contents)
    (def absdest (os/realpath absdest))
    (array/push files absdest)
    (when chmod-mode
      (os/chmod absdest chmod-mode))
    (print "add " absdest)
    absdest))

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

(defn run-tests
  "Run tests on a project in the current directory. The tests will
  be run in the environment dictated by (dyn :modpath)."
  [&opt root-directory]
  (var errors-found 0)
  (defn dodir
    [dir]
    (each sub (sort (os/dir dir))
      (def ndir (string dir "/" sub))
      (case (os/stat ndir :mode)
        :file (when (string/has-suffix? ".janet" ndir)
                (print "running " ndir " ...")
                (flush)
                (def result (sh/exec (dyn *executable* "janet") "--" ndir))
                (when (not= 0 result)
                  (++ errors-found)
                  (eprinf (color :red "non-zero exit code in %s: ") ndir)
                  (eprintf "%d" result)))
        :directory (dodir ndir))))
  (dodir (or root-directory "test"))
  (if (zero? errors-found)
    (print (color :green "✓ All tests passed."))
    (do
      (prin (color :red "✘ Failing test scripts: "))
      (printf "%d" errors-found)
      (os/exit 1)))
  (flush))

###
### Declare stubs
###

(defn declare-project
  "Define your project metadata. This should
  be the first declaration in a project.janet file.
  Also sets up basic task targets like clean, build, test, etc."
  [&named name description url version repo tag dependencies]
  (assert name)
  (assert description)
  (def rules (get-rules))
  (def bd (build-dir))
  (build-rules/build-rule
    rules :pre-build []
    (os/mkdir bd)
    (os/mkdir (path/join bd "static")))
  (build-rules/build-rule
    rules :clean []
    (print "removing directort " bd)
    (sh/rm bd))
  (build-rules/build-rule
    rules :install ["build"])
  (build-rules/build-rule
    rules :build ["pre-build"])
  (build-rules/build-rule
    rules :test ["build"]
    (run-tests))
  (build-rules/build-rule
    rules :check ["build"]
    (run-tests)))

(defn declare-source
  "Create Janet modules. This does not actually build the module(s),
  but registers them for packaging and installation. :source should be an
  array of files and directores to copy into JANET_MODPATH or JANET_PATH.
  :prefix can optionally be given to modify the destination path to be
  (string JANET_PATH prefix source)."
  [&named source prefix]
  (if (bytes? source)
    (install-rule source prefix)
    (each s source
      (install-rule s prefix))))

(defn declare-headers
  "Declare headers for a library installation. Installed headers can be used by other native
  libraries."
  [&named headers prefix]
  (if (bytes? headers)
    (install-rule headers (if prefix prefix))
    (each s headers
      (def bn (path/basename s))
      (def dest (if prefix (path/join prefix bn) bn))
      (install-rule s dest))))

(defn declare-bin
  "Declare a generic file to be installed as an executable."
  [&named main]
  (install-rule main "bin"))

(defn declare-binscript
  ``Declare a janet file to be installed as an executable script. Creates
  a shim on windows. If hardcode is true, will insert code into the script
  such that it will run correctly even when JANET_PATH is changed. if auto-shebang
  is truthy, will also automatically insert a correct shebang line.
  ``
  [&named main hardcode-syspath is-janet]
  (def auto-shebang is-janet)
  (def main (path/abspath main))
  (defn contents []
    (with [f (file/open main :rbn)]
      (def first-line (:read f :line))
      (def second-line (string/format "(put root-env :syspath %v)\n" (dyn *syspath*)))
      (def rest (:read f :all))
      (string (if auto-shebang
                (string "#!/usr/bin/env janet\n"))
              first-line (if hardcode-syspath second-line) rest)))
  (install-buffer contents (path/join "bin" (path/basename main)))
  (when (is-win-or-mingw)
    (def bat (string "@echo off\r\ngoto #_undefined_# 2>NUL || title %COMSPEC% & janet \"" main "\" %*"))
    (def newname (string main ".bat"))
    (install-buffer bat (path/basename newname))))

(defn declare-archive
  "Build a janet archive. This is a file that bundles together many janet
  scripts into a janet image. This file can the be moved to any machine with
  a janet vm and the required dependencies and run there."
  [&named entry name deps]
  (def iname (string name ".jimage"))
  (def ipath (path/join (build-dir) iname))
  (default deps @[])
  (def rules (get-rules))
  (build-rules/build-rule
    rules ipath deps
    (sh/create-dirs-to iname)
    (spit iname (make-image (require entry))))
  (build-rules/build-rule
    rules :build [ipath])
  (install-rule ipath iname))

(defn declare-manpage
  "Mark a manpage for installation"
  [page]
  (install-rule page "man"))

(defn declare-native
  "Declare a native module. This is a shared library that can be loaded
  dynamically by a janet runtime. This also builds a static libary that
  can be used to bundle janet code and native into a single executable."
  [&named name source embedded lflags libs cflags
   c++flags defines install nostatic static-libs
   use-rpath use-rdynamic pkg-config-flags
   pkg-config-libs smart-libs c-std c++-std target-os]

  (def rules (get-rules))

  # Defaults
  (default embedded @[])
  (default nostatic false)
  (default defines @{})
  (default smart-libs false)

  # Create build environment table
  (def benv @{cc/*build-dir* (build-dir)
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

  # TODO - handle cpp
  (def has-cpp false)

  (with-env benv
    (def to (cc/out-path name suffix))
    (install-rule to (string name suffix))
    (def objects @[])
    (loop [src :in embedded]
      (def c-src (cc/out-path src ".c"))
      (def o-src (cc/out-path src ".o"))
      (array/push objects o-src)
      (build-rules/build-rule rules c-src [src]
                              (create-buffer-c (slurp src) c-src (embed-name src)))
      (compile-c c-src o-src))
    (cc to ;source ;objects)
    (build-rules/build-rule rules :build [to])
    (unless nostatic
      (def toa (cc/out-path name asuffix))
      (install-rule toa (string name asuffix))
      (with-dyns [cc/*build-dir* "build/static"]
        (def sobjects @[])
        (loop [src :in embedded]
          (def c-src (cc/out-path src ".c"))
          (def o-src (cc/out-path src ".o"))
          (array/push sobjects o-src)
          (build-rules/build-rule rules c-src [src]
                                  (create-buffer-c (slurp src) c-src (embed-name src)))
          (compile-c c-src o-src))
        (static-cc toa ;source ;sobjects)
        (build-rules/build-rule rules :build [toa]))))

  (unless nostatic
    (def metaname (string name ".meta.janet"))
    (def ename (entry-name name))
    (defn contents []
      (string/format
        "# Metadata for static library %s\n\n%.20p"
        (string name asuffix)
        {:static-entry ename
         :cpp has-cpp
         :ldflags libs
         :lflags lflags
         :static-libs static-libs
         :smart-libs smart-libs
         :use-rdynamic use-rdynamic
         :use-rpath use-rpath}))
    (install-buffer contents metaname))

  rules)
