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
(import ./build-rules)
(import ./stream)

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
(defdyn *msvc-vcvars* "Path to vcvarsall.bat to use initialize MSVC environment. If unset, `msvc-find` will try to guess using typical install locations.")
(defdyn *msvc-cpath* "Path to Janet libraries and headers.")
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
(defdyn *rules* "Rules to use with visit-add-rule")
(defdyn *vcvars-cache* "Where to cache vcvars once we have calculated them")
(defdyn *janet-prefix* "Path prefix used to detect where to find libjanet, janet.h, etc.")

###
### Prefix detection
###

(defn get-unix-prefix
  "Auto-detect what prefix to use for finding libjanet.so, headers, etc. on unix systems"
  []
  (if-let [p (dyn *janet-prefix*)] (break p))
  (var result nil)
  (each test [(os/getenv "JANET_PREFIX")
              (os/getenv "PREFIX")
              (path/join (dyn *syspath*) ".." "..")
              (path/join (dyn *syspath*) "..")
              (try (path/join (sh/self-exe) ".." "..") ([e] nil))
              (dyn *syspath*)
              "/usr/"
              "/usr/local"
              "/"]
    (when test
      (def headercheck (path/join test "include" "janet.h"))
      (when (= :file (os/stat headercheck :mode))
        (set result test)
        (break))))
  (assert result "no prefix discovered for janet headers!")
  (setdyn *janet-prefix* result)
  result)

(defn get-msvc-prefix
  "Auto-detect install location on windows systems with a default install. This is the directory containing Library, C, docs, bin, etc."
  []
  (if-let [p (dyn *janet-prefix*)] (break p))
  (var result nil)
  (each test [(os/getenv "JANET_PREFIX")
              (os/getenv "PREFIX")
              (path/join (dyn *syspath*) ".." "..")
              (path/join (dyn *syspath*) "..")
              (try (path/join (sh/self-exe) ".." "..") ([e] nil))
              (dyn *syspath*)]
    (when test
      (def headercheck (path/join test "C" "janet.h"))
      (when (= :file (os/stat headercheck :mode))
        (set result test)
        (break))))
  (assert result "no prefix discovered for janet headers!")
  (setdyn *janet-prefix* result)
  result)

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
(defn- vcvars-cache [] (dyn *vcvars-cache* ".vcvars.jdn"))

(defn- build-type []
  (def bt (dyn *build-type* :develop))
  (if-not (in {:develop true :debug true :release true} bt)
    (errorf "invalid build type %v, expected :release, :develop, or :debug" bt))
  bt)

(defn- lib-path []
  "Guess a library path based on the current system path"
  (def prefix (get-unix-prefix))
  (path/join prefix "lib"))

(defn- include-path []
  "Guess a header path based on the current system path"
  (def prefix (get-unix-prefix))
  (path/join prefix "include"))

(defn- msvc-cpath
  "Guess a library and header path for msvc with a defualt Janet windows install."
  []
  (when-let [p (dyn *msvc-cpath*)] (break p))
  (def wp (get-msvc-prefix))
  (path/join wp "C"))

(defn msvc-janet-import-lib
  "Get path to the installed Janet import lib. This import lib is needed when create dlls for natives."
  []
  (path/join (msvc-cpath) "janet.lib"))

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
  "Classify a source file as C or C++ (or object files)"
  [path]
  (cond
    (string/has-suffix? ".c" path) :c
    (string/has-suffix? ".cc" path) :c++
    (string/has-suffix? ".cpp" path) :c++
    (string/has-suffix? ".cxx" path) :c++
    # object files
    (string/has-suffix? ".o" path) :o
    (string/has-suffix? ".obj" path) :o
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
    (if (= v true)
      (array/push res (string "-D" k))
      (array/push res (string "-D" k "=" v))))
  (sort res) # for deterministic builds
  res)
(defn- extra-paths []
  (def sp (dyn *syspath* "."))
  (def ip (include-path))
  [(string "-I" sp)
   # ;(if (dyn :verbose) ["-v"] []) # err, too verbose
   ;(if (and ip (not= ip sp)) [(string "-I" ip)] [])])
(defn- extra-link-paths []
  (def sp (dyn *syspath* "."))
  (def lp (lib-path))
  [(string "-L" sp)
   ;(if (and lp (not= lp sp)) [(string "-L" lp)] [])])
(defn- rpath
  []
  (if (dyn *use-rpath* true)
    [(string "-Wl,-rpath," (lib-path))
     (string "-Wl,-rpath," (dyn *syspath* "."))]
    []))
(defn- smart-libs []
  (def dflt (index-of (target-os) [:linux]))
  (dyn *smart-libs* dflt))
(defn- libs [static]
  (def dl (if (= (target-os) :macos) ["-undefined" "dynamic_lookup"] []))
  (def sg (if (smart-libs) ["-Wl,--start-group"] []))
  (def eg (if (smart-libs) ["-Wl,--end-group"] []))
  (def bs (if (not= (target-os) :macos) ["-Wl,-Bstatic"] []))
  (def bd (if (not= (target-os) :macos) ["-Wl,-Bdynamic"] []))
  [;sg
   ;(lflags)
   ;(if static ["-static"] [])
   ;dl
   ;(default-libs)
   ;bs
   ;(static-libs)
   ;bd
   ;(dynamic-libs)
   ;(if static bs []) # put back to static linking so the -static flag works.
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
  "Compile a C source file to an object file. Return the command arguments."
  [from to]
  (exec [(cc) (ccstd) ;(opt) ;(cflags) ;(extra-paths) "-fPIC" ;(defines) "-c" from "-o" to "-pthread"]
        [from] [to] (string "compiling " from "...")))

(defn compile-c++
  "Compile a C++ source file to an object file. Return the command arguments."
  [from to]
  (exec [(c++) (c++std) ;(opt) ;(c++flags) ;(extra-paths) "-fPIC" ;(defines) "-c" from "-o" to "-pthread"]
        [from] [to] (string "compiling " from "...")))

(defn link-shared-c
  "Link a C program to make a shared library. Return the command arguments."
  [objects to]
  (exec [(cc) (ccstd) ;(opt) ;(cflags) ;(extra-link-paths) "-o" to ;objects "-pthread" ;(libs false) ;(dynamic-libs) "-shared"]
        objects [to] (string "linking " to "...")))

(defn link-shared-c++
  "Link a C++ program to make a shared library. Return the command arguments."
  [objects to]
  (exec [(c++) (c++std) ;(opt) ;(c++flags) ;(extra-link-paths) "-o" to ;objects "-pthread" ;(libs false) ;(dynamic-libs) "-shared"]
        objects [to] (string "linking " to "...")))

(defn link-executable-c
  "Link a C program to make an executable. Return the command arguments."
  [objects to &opt make-static]
  (exec [(cc) (ccstd) ;(opt) ;(cflags) ;(extra-link-paths) "-o" to ;objects ;(rdynamic) "-pthread" ;(libs make-static)]
        objects [to] (string "linking " to "...")))

(defn link-executable-c++
  "Link a C++ program to make an executable. Return the command arguments."
  [objects to &opt make-static]
  (exec [(c++) (c++std) ;(opt) ;(c++flags) ;(extra-link-paths) "-o" to ;objects ;(rdynamic) "-pthread" ;(libs make-static)]
        objects [to] (string "linking " to "...")))

(defn make-archive
  "Make an archive file. Return the command arguments."
  [objects to]
  (exec [(ar) "rcs" to ;objects] objects [to] (string "archiving " to "...")))

# Compound commands

(defn out-path
  "Take a source file path and convert it to an output path with no intermediate directories."
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
  [sources cmds-into ext]
  (def objects @[])
  (var has-cpp false)
  (each source sources
    (def o (out-path source ext))
    (def source-type (classify-source source))
    (case source-type
      :o
      (array/push objects source)
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
  (def [has-cpp objects] (compile-many sources res ".shared.o"))
  (array/push
    res
    (if has-cpp
      (link-shared-c++ objects to)
      (link-shared-c objects to))))

(defn compile-and-link-executable
  "Compile and link an executable C/C++ program. Return an array of commands."
  [to & sources]
  (def res @[])
  (def [has-cpp objects] (compile-many sources res ".executable.o"))
  (array/push
    res
    (if has-cpp
      (link-executable-c++ objects to)
      (link-executable-c objects to))))

(defn compile-and-make-archive
  "Compile and create a static archive. Return an array of commands."
  [to & sources]
  (def res @[])
  (def [_ objects] (compile-many sources res ".static.o"))
  (array/push res (make-archive objects to)))

###
### MSVC Compiler Wrapper (msvc 2017 and later)
###

### TODO:
### - more testing
### - libraries

(def- tag "AAAAAAAAAAAAAAAAAAAAAAAAAAA")
(def- vcvars-grammar
  (peg/compile
    ~{:main (* (thru ,tag) (any :line))
      :line (group (* :key "=" :value :s*))
      :key '(to "=")
      :value '(to "\n")}))

(defn msvc-setup?
  "Check if MSVC environment is already setup."
  []
  (and
    (os/getenv "INCLUDE")
    (os/getenv "LIB")
    (os/getenv "LIBPATH")))

(defn- dumb-escape
  [x]
  (->> x
       (string/replace-all " " "^ ")
       (string/replace-all "(" "^(")
       (string/replace-all ")" "^)")))

(defn msvc-find
  ``Find vcvarsall.bat and run it to setup the current environment for building.
  Uses `(dyn *msvc-vcvars*)` to find the location of the setup script, then will check
  for the presence of a file `(dyn *vcvars-cache* ".vcvars.jdn")`, and then otherwise defaults
  to checking typical install locations for Visual Studio.
  Supports VS 2017, 2019, and 2022, any edition.
  Will set environment variables such that invocations of cl.exe, link.exe, etc.
  will work as expected.``
  []
  (when (msvc-setup?) (break))
  # Cache the vcvars locally instead of calling vcvarsall.bat over and over again
  (var found false)
  (when-with [f (file/open (vcvars-cache))]
    (def data (-> f (:read :all) parse))
    (eachp [k v] data
      (os/setenv k v))
    (set found true))
  (if found (break))
  (def arch (string (os/arch)))
  (defn loc [pf y e]
    (string `C:\` pf `\Microsoft Visual Studio\` y `\` e `\VC\Auxiliary\Build\vcvarsall.bat`))
  (var found-path nil)
  (if-let [vcv (dyn *msvc-vcvars*)]
    (set found-path vcv)
    (do
      (loop [pf :in ["Program Files" "Program Files (x86)"]
             y :in [2022 2019 2017]
             e :in ["Enterprise" "Professional" "Community" "BuildTools"]]
        (def path (loc pf y e))
        (when (os/stat path :mode)
          (set found-path path)
          (break)))))
  (unless found-path (error "Could not find vcvarsall.bat"))
  (when (dyn :verbose)
    (print "found " found-path))
  (def arg (string (dumb-escape found-path) ` ` arch ` && echo ` tag ` && set`))
  (def output (sh/exec-slurp "cmd" "/s" "/c" arg))
  (def kvpairs (peg/match vcvars-grammar output))
  (assert kvpairs)
  (def cache @{})
  (each [k v] kvpairs
    (def kk (string/trim k))
    (def vv (string/trim v))
    (put cache kk vv)
    (os/setenv kk vv))
  (spit (vcvars-cache) (string/format "%j" cache))
  nil)

(defn- msvc-opt
  []
  (case (build-type)
    :debug ["/Od" "/DDEBUG" "/Z7" "/MDd"]
    :develop ["/O2" "/DDEBUG" "/Z7" "/MDd"]
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
  (def sp (dyn *syspath* "."))
  (if (= sp cpath)
    [(string "/I" cpath)]
    [(string "/I" cpath) (string "/I" sp)]))
(defn- msvc-link-paths
  []
  (def cpath (msvc-cpath))
  (def sp (dyn *syspath* "."))
  (if (= sp cpath)
    [(string "/LIBPATH:" cpath)]
    [(string "/LIBPATH:" cpath) (string "/LIBPATH:" sp)]))
(defn- msvc-libs []
  (seq [l :in (dyn *msvc-libs* [])]
    (if (string/has-suffix? ".lib" l)
      l
      (string l ".lib"))))
(defn- cl.exe [] "cl.exe")
(defn- link.exe [] "link.exe")
(defn- lib.exe [] "lib.exe")

(defn msvc-compile-c
  "Compile a C source file with MSVC to an object file. Return the command arguments."
  [from to]
  (exec [(cl.exe) "/c" (msvc-cstd) "/utf-8" "/nologo" ;(cflags) ;(msvc-compile-paths) ;(msvc-opt) ;(msvc-defines)
         from (string "/Fo" to)]
        [from] [to] (string "compiling " from "...")))

(defn msvc-compile-c++
  "Compile a C++ source file with MSVC to an object file. Return the command arguments."
  [from to]
  (exec [(cl.exe) "/c" (msvc-c++std) "/utf-8" "/nologo" "/EHsc" ;(c++flags) ;(msvc-compile-paths) ;(msvc-opt) ;(msvc-defines)
         from (string "/Fo" to)]
        [from] [to] (string "compiling " from "...")))

(defn msvc-link-shared
  "Link a C/C++ program with MSVC to make a shared library. Return the command arguments."
  [objects to]
  (exec [(link.exe) "/nologo" "/DLL" (string "/OUT:" to) ;objects ;(msvc-link-paths) ;(msvc-libs) ;(lflags)]
        objects [to] (string "linking " to "...")))

(defn msvc-link-executable
  "Link a C/C++ program with MSVC to make an executable. Return the command arguments."
  [objects to &opt make-static]
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
  [sources cmds-into ext]
  (def objects @[])
  (each source sources
    (def o (out-path source ext "\\"))
    (def source-type (classify-source source))
    (case source-type
      :o
      (array/push objects source)
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
  (def objects (msvc-compile-many sources res ".shared.o"))
  (array/push
    res
    (msvc-link-shared objects to)))

(defn msvc-compile-and-link-executable
  "Compile and link an executable C/C++ program. Return an array of commands."
  [to & sources]
  (def res @[])
  (def objects (msvc-compile-many sources res ".executable.o"))
  (array/push
    res
    (msvc-link-executable objects to)))

(defn msvc-compile-and-make-archive
  "Compile and create a static archive. Return an array of commands."
  [to & sources]
  (def res @[])
  (def objects (msvc-compile-many sources res ".static.o"))
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
  (print ".PHONY: _all")
  (print "_all: " (string/join outputs " "))
  (print (first outputs) ": " (string/join inputs " "))
  (print "\t@echo " (describe message))
  (print "\t@'" (string/join cmd "' '") "'\n"))

(def ver (tuple ;(map scan-number (string/split "." janet/version))))

(defn- exec-linebuffered
  "Line buffer compiler output so we can run commands in parallel"
  [args]
  (when (< ver [1 36 0]) # os/pipe with flags is a new feature
    (break (eprint (sh/exec-slurp ;args))))
  (def [r w] (os/pipe :W))
  (def proc (os/spawn args :p {:out w :err w}))
  (var exit nil)
  (ev/gather
    (each line (stream/lines r)
      (eprint line))
    (do
      (set exit (os/proc-wait proc))
      (ev/close w)))
  (if (not= 0 exit) (error "non-zero exit code"))
  exit)

(defn visit-execute
  "A function that can be provided as `(dyn *visit*)` that will execute commands."
  [cmd inputs outputs message]
  (if (dyn :verbose)
    (do
      (eprint (string/join cmd " "))
      (flush)
      (exec-linebuffered cmd))
    (do
      (print message)
      (with [proc (os/spawn cmd :p {:out :pipe :err :pipe})]
        (def [out err exit] (ev/gather
                              (ev/read (proc :out) :all)
                              (ev/read (proc :err) :all)
                              (os/proc-wait proc)))
        (unless (zero? exit) # only print output on failure
          (if out (eprint (string/trimr out)))
          (if err (eprint (string/trimr err)))
          (error "non-zero exit code"))))))

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
  (with [devnull (sh/devnull)]
    (os/execute cmd :px {:out devnull :err devnull})))

(defn visit-add-rule
  "Used in conjuction with spork/build-rules. Adds rules to the (dyn *rules* (curenv))"
  [cmd inputs outputs message]
  (def rules (dyn *rules* (curenv)))
  (build-rules/build-rule
    rules outputs @[;inputs]
    (visit-execute cmd inputs outputs message)))

###
### Library discovery and self check
###

(defn check-library-exists
  "Check if a library exists on the current POSIX system. Will run a test compilation
  and return true if the compilation succeeds. Libname is passed directly to the compiler/linker, such as `-lm` on GNU/Linux."
  [libname &opt binding test-source-code]
  (default binding *libs*)
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
          (with [devnull (sh/devnull)]
            (os/execute [executable] :x {:out devnull :err devnull}))
          true)
        ([e]
          false)))))

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
  (setdyn *cflags* (array/concat @[] (getsetdyn *cflags*) cflags))
  (setdyn *lflags* (array/concat @[] (getsetdyn *lflags*) lflags))
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
