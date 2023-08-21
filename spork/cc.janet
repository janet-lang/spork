###
### cc.janet
###
### Wrapper around the system C compiler for compiling Janet native modules and executables.
### Opinionated and optimized for use with Janet, and does not actually run
### commands unless specified with (dyn *visit*).
### Headers, static libraries, and dynamic libraries can all be used from `(dyn *syspath*)`.
###
### Configuration is done via dynamic variables, so normal usage would look like:
###
### (use spork/cc)
### (with-dyns [*defines* {"JANET_MODULE_ENTRY" "spork_crc_module_entry"}
###             *cflags* ["-g"]
###             *visit* (fn [cmd &] (-> cmd tracev (os/execute :px)))]
###   (compile-c "src/crc.c" "crc.o")
###   (link-shared-c ["crc.o"] "crc.so"))

(defdyn *ar* "Archiver, defaults to `ar`.")
(defdyn *build-dir* "If generating intermediate files, store them in this directory")
(defdyn *build-type* "Presets for compiler optimizations, can be :release, :develop, and :debug, defaults to :develop.")
(defdyn *c++* "C++ compiler, defaults to `c++`.")
(defdyn *c++flags* "Extra C++ compiler flags to use during compilation")
(defdyn *cc* "C compiler, defaults to `cc`.")
(defdyn *cflags* "Extra C compiler flags to use during compilation")
(defdyn *defines* "Map of extra defines to use when compiling")
(defdyn *dynamic-libs* "List of dynamic libraries to use when compiling")
(defdyn *lflags* "Extra linker flags")
(defdyn *static-libs* "List of static libraries to use when compiling")
(defdyn *target-os* "Operating system to assume is being used for target compiler toolchain")
(defdyn *visit* "Optional callback to process each CLI command and its inputs and outputs")

# Utilties to reference dynamic variables with defaults
(defn- target-os [] (dyn *target-os* (os/which)))
(defn- build-type []
  (def bt (dyn *build-type* :develop))
  (if-not (in {:develop true :debug true :release true} bt)
    (errorf "invalid build type %v, expected :release, :develop, or :debug" bt))
  bt)
(defn- ar [] (dyn *ar* "ar"))
(defn- cc [] (dyn *cc* "cc"))
(defn- c++ [] (dyn *c++* "c++"))
(defn- opt [] (case (build-type)
                :debug "-O0"
                "-O2"))
(defn- g [] (case (build-type) :release [] ["-g"]))
(defn- cflags [] (dyn *cflags* []))
(defn- c++flags [] (dyn *c++flags* []))
(defn- lflags [] (dyn *lflags* []))
(defn- defines []
  (def res @[])
  (array/push res (string "-DJANET_BUILD_TYPE=" (build-type)))
  (eachp [k v] (dyn *defines* [])
    (array/push res (string "-D" k "=" v)))
  (sort res) # for deterministic builds
  res)
(defn- extra-paths []
  (def sp (dyn *syspath*))
  [(string "-I" sp) (string "-L" sp)])
(defn- static-libs [] (dyn *static-libs* []))
(defn- dynamic-libs [] (dyn *dynamic-libs* []))
(defn- libs [] [;(lflags) "-Wl,-Bstatic" ;(static-libs) "-Wl,-Bdynamic" ;(dynamic-libs)])
(defn- default-exec [cmd &] cmd)
(defn- exec
  "Call the (dyn *visit*) function on commands"
  [cmd inputs outputs message]
  ((dyn *visit* default-exec) cmd inputs outputs message) cmd)
(defn- rdynamic
  "Some systems like -rdynamic, some like -Wl,-export_dynamic"
  []
  (if (= (target-os) :macos) "-Wl,-export_dynamic" "-rdynamic"))
(defn- ccstd [] "-std=c99")
(defn- c++std [] "-std=c++11")
(defn- build-dir [] (dyn *build-dir* "."))

###
### Basic Compiler Wrapper
###

(defn compile-c
  "Compile a C program to an object file. Return the command arguments."
  [from to]
  (exec [(cc) (ccstd) (opt) ;(extra-paths) "-fPIC" ;(cflags) ;(g) ;(defines) "-c" from "-o" to "-pthread"]
        [from] [to] (string "compiling " from "...")))

(defn compile-c++
  "Compile a C++ program to an object file. Return the command arguments."
  [from to]
  (exec [(c++) (c++std) (opt) ;(extra-paths) "-fPIC" ;(c++flags) ;(g) ;(defines) "-c" from "-o" to "-pthread"]
        [from] [to] (string "compiling " from "...")))

(defn link-shared-c
  "Link a C program to make a shared library. Return the command arguments."
  [objects to]
  (exec [(cc) (ccstd) (opt) ;(extra-paths) ;(cflags) ;(g) "-o" to ;objects "-pthread" ;(libs) "-shared"]
        objects [to] (string "linking " to "...")))

(defn link-shared-c++
  "Link a C++ program to make a shared library. Return the command arguments."
  [objects to]
  (exec [(c++) (c++std) (opt) ;(extra-paths) ;(c++flags) ;(g) "-o" to ;objects "-pthread" ;(libs) "-shared"]
        objects [to] (string "linking " to "...")))

(defn link-executable-c
  "Link a C program to make an executable. Return the command arguments."
  [objects to]
  (exec [(cc) (ccstd) (opt) ;(extra-paths) ;(cflags) ;(g) "-o" to ;objects (rdynamic) "-pthread" ;(libs)]
        objects [to] (string "linking " to "...")))

(defn link-executable-c++
  "Link a C program to make an executable. Return the command arguments."
  [objects to]
  (exec [(c++) (c++std) (opt) ;(extra-paths) ;(c++flags) ;(g) "-o" to ;objects (rdynamic) "-pthread" ;(libs)]
        objects [to] (string "linking " to "...")))

(defn make-archive
  "Make an archive file. Return the command arguments."
  [objects to]
  (exec [(ar) "rcs" to ;objects] objects [to] (string "archiving " to "...")))

# Compound commands

(defn- out-path
  "Take a source file path and convert it to an output path."
  [path from-ext to-ext]
  (->> path
       (string/replace-all "\\" "___")
       (string/replace-all "/" "___")
       (string/replace-all from-ext to-ext)
       (string (build-dir) "/")))

(defn- compile-many
  "Compile a number of source files, and return the
  generated objects files, as well as a boolean if any cpp
  source files were found."
  [sources cmds-into]
  (def objects @[])
  (var has-cpp false)
  (each source sources
    (cond
      (string/find ".cpp" source)
      (do
        (set has-cpp true)
        (def o (out-path source ".cpp" ".o"))
        (array/push cmds-into (compile-c++ source o))
        (array/push objects o))
      (string/find ".cc" source)
      (do
        (set has-cpp true)
        (def o (out-path source ".cc" ".o"))
        (array/push cmds-into (compile-c++ source o))
        (array/push objects o))
      # else
      (do
        (def o (out-path source ".c" ".o"))
        (array/push cmds-into (compile-c source o))
        (array/push objects o))))
  [has-cpp objects])

(defn compile-and-link-shared
  "Compile and link a shared C/C++ program. Return an array of commands."
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
### *visit* premade functions
###

(defn visit-generate-makefile
  "A visiting function that can be used to generate Makefile targets"
  [cmd inputs outputs message]
  (assert (one? (length outputs)) "only single outputs are supported for Makefile generation")
  (print (first outputs) ": " (string/join inputs " "))
  (print "\t@echo " (describe message))
  (print "\t@'" (string/join cmd "' '") "'\n"))

(defn visit-execute-if-stale
  "A function that can be provided as (dyn *visit*) that will execute a command
  if inputs are newer than outputs, providing a simple, single-threaded, incremental build tool."
  [cmd inputs outputs message]
  (defn otime [file] (or (os/stat file :modified) 0))
  (defn itime [file] (or (os/stat file :modified) (errorf "%v: input file %v does not exist!" message file)))
  (def im (max ;(map itime inputs)))
  (def om (min ;(map otime outputs)))
  (if (>= om im) (break))
  (if (dyn :verbose)
    (print (string/join cmd " "))
    (print message))
  (os/execute cmd :px))
