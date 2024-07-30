###
### cc.janet
###
### Improved version of the C Compiler abstraction from JPM that should be more correct, composable, and
### have less configuration.
###
### Wrapper around the system C compiler for compiling Janet native modules and executables.
### Opinionated and optimized for use with Janet, and does not actually run
### commands unless specified with (dyn *visit*). Also included is package config integration.
### Headers, static libraries, and dynamic libraries can all be used from `(dyn *syspath*)`.
###
### Example usage:
###
### (use spork/cc)
###
### (search-static-libraries "m" "rt" "dl")
### (search-dynamic-libraries "janet")
### (pkg-config "sdl2" "vulkan")
### (with-dyns [*defines* {"GAME_BUILD" "devel-0.0"}
###             *visit* visit-execute-if-stale]
###   (compile-and-link-executable "game" "main.c" "sound.c" "graphics.c"))
###

(import ./path)
(import ./sh)

(defdyn *ar* "Archiver, defaults to `ar`.")
(defdyn *build-dir* "If generating intermediate files, store them in this directory")
(defdyn *build-type* "Presets for compiler optimizations, can be :release, :develop, and :debug, defaults to :develop.")
(defdyn *c++* "C++ compiler, defaults to `c++`.")
(defdyn *c++flags* "Extra C++ compiler flags to use during compilation")
(defdyn *cc* "C compiler, defaults to `cc`.")
(defdyn *cflags* "Extra C compiler flags to use during compilation")
(defdyn *defines* "Map of extra defines to use when compiling")
(defdyn *libs* "List of libraries to use when compiling - can be static or dynamic depending on system.")
(defdyn *dynamic-libs* "List of dynamic libraries to use when compiling")
(defdyn *msvc-libs* "List of .lib libraries to use when compiling with msvc")
(defdyn *lflags* "Extra linker flags")
(defdyn *static-libs* "List of static libraries to use when compiling")
(defdyn *target-os* "Operating system to assume is being used for target compiler toolchain")
(defdyn *visit* "Optional callback to process each CLI command and its inputs and outputs")
(defdyn *use-rpath* "Optional setting to enable using `(dyn *syspath*)` as the runtime path to load for Shared Objects. Defaults to true")
(defdyn *use-rdynamic*
  ``Optional setting to enable using `-rdynamic` or `-Wl,-export_dynamic` when linking executables.
  This is the preferred way on POSIX systems to let an executable load native modules dynamically at runtime.
  Defaults to true``)
(defdyn *pkg-config-flags* "Extra flags to pass to pkg-config")
(defdyn *smart-libs*
  ``Try to resolve circular or out-of-order dependencies between libraries by using --start-group and --end-group.
  Some linkers support this by default, but not at all. Defaults to true on linux and macos.``)
(defdyn *c-std* "C standard to use as a 2 digit number, defaults to 99 on GCC-like compilers, 11 on msvc.")
(defdyn *c++-std* "C++ standard to use as a 2 digit number, defaults to 11 on GCC-like compilers, 14 on msvc.")

###
### Universal helpers for all toolchains
###

(defn- cflags [] (dyn *cflags* []))
(defn- c++flags [] (dyn *c++flags* []))
(defn- lflags [] (dyn *lflags* []))
(defn- target-os [] (dyn *target-os* (os/which)))
(defn- build-dir [] (dyn *build-dir* "."))
(defn- static-libs [] (dyn *static-libs* []))
(defn- dynamic-libs [] (dyn *dynamic-libs* []))
(defn- default-libs [] (dyn *libs* []))

(defn- build-type []
  (def bt (dyn *build-type* :develop))
  (if-not (in {:develop true :debug true :release true} bt)
    (errorf "invalid build type %v, expected :release, :develop, or :debug" bt))
  bt)

(defn- lib-path []
  "Guess a library path based on the current system path"
  (def sp (dyn *syspath* "."))
  (def parts (filter next (path/parts sp)))
  (if (= "janet" (last parts))
    (path/abspath (string sp "/.."))))

(defn- include-path []
  "Guess a header path based on the current system path"
  (def lp (lib-path))
  (if lp
    (path/join lp "../include")))

(defn- msvc-cpath
  "Guess a library and header path for msvc with a defualt Janet windows install."
  []
  (def sp (dyn *syspath* "."))
  (def parts (filter next (path/parts sp)))
  (if (= "Library" (last parts))
    (path/abspath (string sp "\\..\\C\\"))))

(defn- default-exec [&])
(defn- exec
  "Call the (dyn *visit*) function on commands"
  [cmd inputs outputs message]
  ((dyn *visit* default-exec) cmd inputs outputs message) cmd)

(defn- getsetdyn
  [sym]
  (def x (dyn sym))
  (if (= nil x)
    (setdyn sym @[])
    x))

(defn- classify-source
  "Classify a source file as C or C++"
  [path]
  (cond
    (string/has-suffix? ".c" path) :c
    (string/has-suffix? ".cc" path) :c++
    (string/has-suffix? ".cpp" path) :c++
    (string/has-suffix? ".cxx" path) :c++
    # else
    (errorf "unknown source file type for %v" path)))

###
### Basic GCC-like Compiler Wrapper
###

# GCC toolchain helpers
(defn- ar [] (dyn *ar* "ar"))
(defn- cc [] (dyn *cc* "cc"))
(defn- c++ [] (dyn *c++* "c++"))
(defn- opt []
  (case (build-type)
    :debug ["-O0" "-g"]
    :develop ["-O2" "-g"]
    :release ["-O2"]
    []))
(defn- defines []
  (def res @[])
  (array/push res (string "-DJANET_BUILD_TYPE=" (build-type)))
  (eachp [k v] (dyn *defines* [])
    (array/push res (string "-D" k "=" v)))
  (sort res) # for deterministic builds
  res)
(defn- extra-paths []
  (def sp (dyn *syspath* "."))
  (def lp (lib-path))
  (def ip (include-path))
  [(string "-I" sp)
   ;(if ip [(string "-I" ip)] [])
   (string "-L" sp)
   ;(if lp [(string "-L" lp)] [])])
(defn- rpath
  []
  (if (dyn *use-rpath* true)
    [(string "-Wl,-rpath," (lib-path))
     (string "-Wl,-rpath," (dyn *syspath* "."))]
    []))
(defn- smart-libs []
  (def dflt (index-of (target-os) [:linux]))
  (dyn *smart-libs* dflt))
(defn- libs []
  (def sg (if (smart-libs) ["-Wl,--start-group"] []))
  (def eg (if (smart-libs) ["-Wl,--end-group"] []))
  (def bs (if (not= (target-os) :macos) ["-Wl,-Bstatic"] []))
  (def bd (if (not= (target-os) :macos) ["-Wl,-Bdynamic"] []))
  [;(lflags)
   ;sg
   ;(default-libs)
   ;bs
   ;(static-libs)
   ;bd
   ;(dynamic-libs)
   ;eg
   ;(rpath)])
(defn- rdynamic
  "Some systems like -rdynamic, some like -Wl,-export_dynamic"
  []
  (if (dyn *use-rdynamic* true)
    [(if (= (target-os) :macos) "-Wl,-export_dynamic" "-rdynamic")]
    []))
(defn- ccstd []
  (def std (dyn *c-std* 99))
  (if (and (bytes? std) (string/has-prefix? "-" std))
    std
    (string "-std=c" std)))
(defn- c++std []
  (def std (dyn *c++-std* 11))
  (if (and (bytes? std) (string/has-prefix? "-" std))
    std
    (string "-std=c++" std)))

(defn compile-c
  "Compile a C program to an object file. Return the command arguments."
  [from to]
  (exec [(cc) (ccstd) ;(opt) ;(extra-paths) "-fPIC" ;(cflags) ;(defines) "-c" from "-o" to "-pthread"]
        [from] [to] (string "compiling " from "...")))

(defn compile-c++
  "Compile a C++ program to an object file. Return the command arguments."
  [from to]
  (exec [(c++) (c++std) ;(opt) ;(extra-paths) "-fPIC" ;(c++flags) ;(defines) "-c" from "-o" to "-pthread"]
        [from] [to] (string "compiling " from "...")))

(defn link-shared-c
  "Link a C program to make a shared library. Return the command arguments."
  [objects to]
  (exec [(cc) (ccstd) ;(opt) ;(extra-paths) ;(cflags) "-o" to ;objects "-pthread" ;(libs) "-shared"]
        objects [to] (string "linking " to "...")))

(defn link-shared-c++
  "Link a C++ program to make a shared library. Return the command arguments."
  [objects to]
  (exec [(c++) (c++std) ;(opt) ;(extra-paths) ;(c++flags) "-o" to ;objects "-pthread" ;(libs) "-shared"]
        objects [to] (string "linking " to "...")))

(defn link-executable-c
  "Link a C program to make an executable. Return the command arguments."
  [objects to]
  (exec [(cc) (ccstd) ;(opt) ;(extra-paths) ;(cflags) "-o" to ;objects ;(rdynamic) "-pthread" ;(libs)]
        objects [to] (string "linking " to "...")))

(defn link-executable-c++
  "Link a C++ program to make an executable. Return the command arguments."
  [objects to]
  (exec [(c++) (c++std) ;(opt) ;(extra-paths) ;(c++flags) "-o" to ;objects ;(rdynamic) "-pthread" ;(libs)]
        objects [to] (string "linking " to "...")))

(defn make-archive
  "Make an archive file. Return the command arguments."
  [objects to]
  (exec [(ar) "rcs" to ;objects] objects [to] (string "archiving " to "...")))

# Compound commands

(defn- out-path
  "Take a source file path and convert it to an output path."
  [path to-ext &opt sep]
  (default sep "/")
  (def flatpath
    (->> path
         (string/replace-all "\\" "___")
         (string/replace-all "/" "___")))
  (string (build-dir) sep flatpath to-ext))

(defn- compile-many
  "Compile a number of source files, and return the
  generated objects files, as well as a boolean if any cpp
  source files were found."
  [sources cmds-into]
  (def objects @[])
  (var has-cpp false)
  (each source sources
    (def o (out-path source ".o"))
    (def source-type (classify-source source))
    (case source-type
      :c
      (do
        (array/push cmds-into (compile-c source o))
        (array/push objects o))
      :c++
      (do
        (set has-cpp true)
        (array/push cmds-into (compile-c++ source o))
        (array/push objects o))
      # else
      (errorf "unknown source file type for %v" source)))
  [has-cpp objects])

(defn compile-and-link-shared
  "Compile and link a shared C/C++ library. Return an array of commands."
  [to & sources]
  (def res @[])
  (def [has-cpp objects] (compile-many sources res))
  (array/push
    res
    (if has-cpp
      (link-shared-c++ objects to)
      (link-shared-c objects to))))

(defn compile-and-link-executable
  "Compile and link an executable C/C++ program. Return an array of commands."
  [to & sources]
  (def res @[])
  (def [has-cpp objects] (compile-many sources res))
  (array/push
    res
    (if has-cpp
      (link-executable-c++ objects to)
      (link-executable-c objects to))))

(defn compile-and-make-archive
  "Compile and create a static archive. Return an array of commands."
  [to & sources]
  (def res @[])
  (def [_ objects] (compile-many sources res))
  (array/push res (make-archive objects to)))

###
### MSVC Compiler Wrapper (msvc 2017 and later)
###

### TODO:
### - more testing
### - libraries

(defn- msvc-opt
  []
  (case (build-type)
    :debug ["/Od" "/Zi" "/MDd"]
    ["/O2" "/MD"]))
(defn- msvc-defines []
  (def res @[])
  (array/push res (string "/DJANET_BUILD_TYPE=" (build-type)))
  (eachp [k v] (dyn *defines* [])
    (array/push res (string "/D" k "=" v)))
  (sort res) # for deterministic builds
  res)
(defn- msvc-cstd
  []
  (def std (dyn *c-std* 11))
  (if (and (bytes? std) (string/has-prefix? "/" std))
    std
    (string "/std:c" std)))
(defn- msvc-c++std
  []
  (def std (dyn *c++-std* 14))
  (if (and (bytes? std) (string/has-prefix? "/" std))
    std
    (string "/std:c++" std)))
(defn- msvc-compile-paths
  []
  (def cpath (msvc-cpath))
  [(string "/I" cpath) (string "/I" (dyn *syspath* "."))])
(defn- msvc-link-paths
  []
  (def cpath (msvc-cpath))
  [(string "/LIBPATH:" cpath) (string "/LIBPATH:" (dyn *syspath* "."))])
(defn- msvc-libs []
  (seq [l :in (dyn *msvc-libs* [])]
    (if (string/has-suffix? ".lib" l)
      l
      (string l ".lib"))))
(defn- cl.exe [] "cl.exe")
(defn- link.exe [] "link.exe")
(defn- lib.exe [] "lib.exe")

(defn msvc-compile-c
  "Compile a C program with MSVC. Return the command arguments."
  [from to]
  (exec [(cl.exe) "/c" (msvc-cstd) "/utf-8" "/nologo" ;(cflags) ;(msvc-compile-paths) ;(msvc-opt) ;(msvc-defines)
         "/I" (dyn *syspath* ".") from (string "/Fo" to)]
        [from] [to] (string "compiling " from "...")))

(defn msvc-compile-c++
  "Compile a C++ program with MSVC. Return the command arguments."
  [from to]
  (exec [(cl.exe) "/c" (msvc-c++std) "/utf-8" "/nologo" "/EHsc" ;(c++flags) ;(msvc-compile-paths) ;(msvc-opt) ;(msvc-defines)
         "/I" (dyn *syspath* ".") from (string "/Fo" to)]
        [from] [to] (string "compiling " from "...")))

(defn msvc-link-shared
  "Link a C/C++ program with MSVC to make a shared library. Return the command arguments."
  [objects to]
  (exec [(link.exe) "/nologo" "/DLL" (string "/OUT:" to) ;objects ;(msvc-link-paths) ;(msvc-libs) ;(lflags)]
        objects [to] (string "linking " to "...")))

(defn msvc-link-executable
  "Link a C/C++ program with MSVC to make an executable. Return the command arguments."
  [objects to]
  (exec [(link.exe) "/nologo" (string "/OUT:" to) ;objects ;(msvc-link-paths) ;(msvc-libs) ;(lflags)]
        objects [to] (string "linking " to "...")))

(defn msvc-make-archive
  "Make an archive file with MSVC. Return the command arguments."
  [objects to]
  (exec [(lib.exe) "/nologo" (string "/OUT:" to) ;objects]
        objects [to] (string "archiving " to "...")))

# Compound commands

(defn- msvc-compile-many
  "Compile a number of source files, and return the
  generated objects files, as well as a boolean if any cpp
  source files were found."
  [sources cmds-into]
  (def objects @[])
  (each source sources
    (def o (out-path source ".o" "\\"))
    (def source-type (classify-source source))
    (case source-type
      :c
      (do
        (array/push cmds-into (msvc-compile-c source o))
        (array/push objects o))
      :c++
      (do
        (array/push cmds-into (msvc-compile-c++ source o))
        (array/push objects o))
      # else
      (errorf "unknown source file type for %v" source)))
  objects)

(defn msvc-compile-and-link-shared
  "Compile and link a shared C/C++ library. Return an array of commands."
  [to & sources]
  (def res @[])
  (def objects (msvc-compile-many sources res))
  (array/push
    res
    (msvc-link-shared objects to)))

(defn msvc-compile-and-link-executable
  "Compile and link an executable C/C++ program. Return an array of commands."
  [to & sources]
  (def res @[])
  (def objects (msvc-compile-many sources res))
  (array/push
    res
    (msvc-link-executable objects to)))

(defn msvc-compile-and-make-archive
  "Compile and create a static archive. Return an array of commands."
  [to & sources]
  (def res @[])
  (def objects (msvc-compile-many sources res))
  (array/push res (msvc-make-archive objects to)))

###
### *visit* functions
###

(defn visit-do-nothing
  "A visiting function that has no side effects and therefor does nothing."
  [&])

(defn visit-clean
  "A visiting function that will remove all outputs."
  [cmd inputs outputs message]
  (print "cleaing " (string/join outputs " ") "...")
  (each output outputs
    (sh/rm output)))

(defn visit-generate-makefile
  "A function that can be provided as `(dyn *visit*)` that will generate Makefile targets."
  [cmd inputs outputs message]
  (assert (one? (length outputs)) "only single outputs are supported for Makefile generation")
  (print (first outputs) ": " (string/join inputs " "))
  (print "\t@echo " (describe message))
  (print "\t@'" (string/join cmd "' '") "'\n"))

(defn visit-execute
  "A function that can be provided as `(dyn *visit*)` that will execute commands."
  [cmd inputs outputs message]
  (if (dyn :verbose)
    (do
      (print (string/join cmd " "))
      (os/execute cmd :px))
    (do
      (print message)
      (def devnull (sh/devnull))
      (os/execute cmd :px {:out devnull :err devnull}))))

(defn visit-execute-if-stale
  "A function that can be provided as `(dyn *visit*)` that will execute a command
  if inputs are newer than outputs, providing a simple, single-threaded, incremental build tool.
  This is not optimal for parallel builds, but is simple and works well for small projects."
  [cmd inputs outputs message]
  (defn otime [file] (or (os/stat file :modified) math/-inf))
  (defn itime [file] (or (os/stat file :modified) (errorf "%v: input file %v does not exist!" message file)))
  (def im (max ;(map itime inputs)))
  (def om (min ;(map otime outputs)))
  (if (>= om im) (break))
  (visit-execute cmd inputs outputs message))

(defn visit-execute-quiet
  "A function that can be provided as `(dyn *visit*)` that will execute commands quietly."
  [cmd inputs outputs message]
  (def devnull (sh/devnull))
  (os/execute cmd :px {:out devnull :err devnull}))

###
### Library discovery and self check
###

(defn check-library-exists
  "Check if a library exists on the current POSIX system. Will run a test compilation
  and return true if the compilation succeeds."
  [libname &opt binding test-source-code]
  (default test-source-code "int main() { return 0; }")
  (def temp (string "_temp" (gensym)))
  (def src (string temp "/" (gensym) ".c"))
  (def executable (string temp "/" (gensym)))
  (defer (sh/rm temp)
    (os/mkdir temp)
    (spit src test-source-code)
    (def result
      (try
        (with-dyns [*visit* visit-execute-quiet
                    *build-dir* temp
                    *static-libs* []
                    *dynamic-libs* []
                    *libs* []]
          (setdyn binding [libname])
          (compile-and-link-executable executable src)
          (def devnull (sh/devnull))
          (os/execute [executable] :x {:out devnull :err devnull})
          true)
        ([e] false)))))

(defn- search-libs-impl
  [dynb libs]
  (def ls (getsetdyn dynb))
  (def notfound @[])
  (each lib libs
    (def llib (if (string/has-prefix? "-l" lib) lib (string "-l" lib)))
    (if (check-library-exists llib dynb)
      (array/push ls llib)
      (array/push notfound lib)))
  notfound)

(defn search-libraries
  "Search for libraries on the current POSIX system and configure `(dyn *libs*)`.
  This is done by checking for the existence of libraries with
  `check-library-exists`. Returns an array of libraries that were not found."
  [& libs]
  (search-libs-impl *libs* libs))

(defn search-static-libraries
  "Search for static libraries on the current POSIX system and configure `(dyn *static-libs*)`.
  This is done by checking for the existence of libraries with
  `check-library-exists`. Returns an array of libraries that were not found."
  [& libs]
  (search-libs-impl *static-libs* libs))

(defn search-dynamic-libraries
  "Search for dynamic libraries on the current POSIX system and configure `(dyn *dynamic-libraries*)`.
  This is done by checking for the existence of libraries with
  `check-library-exists`. Returns an array of libraries that were not found."
  [& libs]
  (search-libs-impl *dynamic-libs* libs))

###
### Package Config wrapper to find libraries and set flags
###

(defn- pkg-config-impl
  [& cmd]
  (def pkg-config-path (or (lib-path) (dyn *syspath* ".")))
  # Janet may be installed in a non-standard location, so we need to tell pkg-config where to look
  (def wp (string "--with-path=" pkg-config-path))
  (def pkp (string "--with-path=" (path/join pkg-config-path "pkgconfig")))
  (def extra (dyn *pkg-config-flags* []))
  (def output (sh/exec-slurp "pkg-config" wp pkp ;extra ;cmd))
  (string/split " " (string/trim output)))

(defn pkg-config
  "Setup defines, cflags, and library flags from pkg-config."
  [& pkg-config-libraries]
  (def cflags (pkg-config-impl "--cflags" ;pkg-config-libraries))
  (def lflags (pkg-config-impl "--libs-only-L" "--libs-only-other" ;pkg-config-libraries))
  (def libs (pkg-config-impl "--libs-only-l" ;pkg-config-libraries))
  (def leftovers (search-libraries ;libs))
  (unless (empty? leftovers)
    (errorf "could not find libraries %j" leftovers))
  (array/concat (getsetdyn *cflags*) cflags)
  (array/concat (getsetdyn *lflags*) lflags)
  nil)

###
### Save and Load configuration
###

# Get all dynamic bindings in this module and make them saveable.
(def- save-map
  (tabseq
    [k :keys (curenv) :when (symbol? k) :when (string/has-prefix? "*" k)]
    (keyword (slice k 1 -2)) true))

(defn save-settings
  "Get a snapshot of the current settings for various compiler flags, libraries, defines, etc. that can be loaded later."
  []
  (freeze
    (tabseq [k :keys save-map]
      k (dyn k))))

(defn load-settings
  "Load settings from a snapshot of settings saved with `save-settings`."
  [settings]
  (eachp [k v] (thaw settings)
    (setdyn k v)))
