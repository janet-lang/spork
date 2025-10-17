###
### declare-cc.janet
###
### Declarative interface for spork/cc and spork/build-rules.
###
### Define builds with (declare-native ...) and (declare-executable ...)
### instead of sequential build recipes. This should also serve as a shorthand for cross platform builds, and
### is less flexible than raw spork/cc.
###

(import ./build-rules)
(import ./cc)
(import ./path)
(import ./sh)
(import ./pm-config)

(defdyn *install-manifest* "Bound to the bundle manifest during a bundle/install.")
(defdyn *toolchain* "Force a given toolchain. If unset, will auto-detect.")
(defdyn *build-root* "Root build directory that will contain all built artifacts")

(defn- build-root [] (dyn *build-root* "_build"))
(defn- build-dir [] (path/join (build-root) (dyn cc/*build-type* :release)))
(defn- get-rules [] (dyn cc/*rules* (curenv)))
(defn- mkbin [] (def x (path/join (dyn *syspath*) "bin")) (fn :make-bin [] (sh/create-dirs x)))
(defn- mkman [] (def x (path/join (dyn *syspath*) "man" "man1")) (fn :make-man [] (sh/create-dirs x)))

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
  (def toolchain (pm-config/detect-toolchain (curenv)))
  (when (= :msvc toolchain)
    (cc/msvc-find)
    (assert (cc/msvc-setup?)))
  toolchain)

(defn- toolchain-to-cc
  [toolchain]
  (case toolchain
    :msvc "cl.exe" # not used but for completion
    :cc "cc"
    :clang "clang"
    :gcc "gcc"
    "cc"))

(defn- toolchain-to-c++
  [toolchain]
  (case toolchain
    :msvc "cl.exe" # not used but for completion
    :cc "c++"
    :clang "clang++"
    :gcc "g++"
    "c++"))

(defn- install-rule
  [src dest &opt chmod-mode thunk is-exe]
  (def rules (get-rules))
  (build-rules/build-rule
    rules :install [src]
    (def manifest (assert (dyn *install-manifest*)))
    (when is-exe
      (put manifest :has-bin-script true) # remove eventually
      (put manifest :has-exe true))
    (when thunk (thunk))
    (bundle/add manifest src dest chmod-mode))
  dest)

(defn- install-buffer
  [contents dest &opt chmod-mode thunk]
  (def rules (get-rules))
  (def contents (if (function? contents) (contents) contents))
  (build-rules/build-rule
    rules :install []
    (def manifest (assert (dyn *install-manifest*)))
    (put manifest :has-bin-script true) # remove eventually
    (put manifest :has-exe true)
    (def files (get manifest :files @[]))
    (put manifest :files files)
    (def absdest (path/join (dyn *syspath*) dest))
    (when (os/stat absdest :mode)
      (errorf "collision at %s, file already exists" absdest))
    (when thunk (thunk))
    (spit absdest contents)
    (def absdest (os/realpath absdest))
    (array/push files absdest)
    (when chmod-mode
      (os/chmod absdest chmod-mode))
    (print "add " absdest)
    absdest))

###
### Rule shorthand
###

(defn- firstt [target] (if (indexed? target) (in target 0) target))
(defn- rule-impl
  [target deps thunk &opt phony]
  (def target (if phony (keyword target) target))
  (def rules (get-rules))
  (build-rules/build-thunk rules target deps thunk))

(defmacro rule
  "Add a rule to the rule graph."
  [target deps & body]
  ~(,rule-impl ,target ,deps (fn ,(keyword (firstt target)) [] nil ,;body)))

(defmacro task
  "Add a task rule to the rule graph. A task rule will always run if invoked
  (it is always considered out of date)."
  [target deps & body]
  ~(,rule-impl ,target ,deps (fn ,(keyword (firstt target)) [] nil ,;body) true))

(defmacro phony
  "Alias for `task`."
  [target deps & body]
  ~(,rule-impl ,target ,deps (fn ,(keyword (firstt target)) [] nil ,;body) true))

(defmacro sh-rule
  "Add a rule that invokes a shell command, and fails if the command returns non-zero."
  [target deps & body]
  ~(,rule-impl ,target ,deps (fn ,(keyword (firstt target)) [] (,sh/exec ,;body))))

(defmacro sh-task
  "Add a task that invokes a shell command, and fails if the command returns non-zero."
  [target deps & body]
  ~(,rule-impl ,target ,deps (fn ,(keyword (firstt target)) [] (,sh/exec ,;body)) true))

(defmacro sh-phony
  "Alias for `sh-task`."
  [target deps & body]
  ~(,rule-impl ,target ,deps (fn ,(keyword (firstt target)) [] (,sh/exec ,;body)) true))

(defn install-file-rule
  "Add install and uninstall rule for moving file from src into destdir."
  [src dest &opt chmod-mode thunk is-exe]
  (install-rule src dest chmod-mode thunk is-exe))

###
### Misc. utilities
###

(def- entry-replacer
  "Convert url with potential bad characters into an entry-name"
  (peg/compile ~(% (any (+ '(range "AZ" "az" "09" "__") (/ '1 ,|(string "_" ($ 0) "_")))))))

(defn- entry-replace
  "Escape special characters in the entry-name"
  [name]
  (get (peg/match entry-replacer name) 0))

(defn- embed-name
  "Rename a janet symbol for embedding."
  [path]
  (->> path
       (string/replace-all "\\" "___")
       (string/replace-all "/" "___")
       (string/replace-all ".janet" "")))

(defn- entry-name
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

(defn- print-rule-tree
  "Show dependencies for a given rule recursively in a nice tree."
  [rules root depth prefix prefix-part]
  (print prefix root)
  (when-let [{:inputs root-deps} (rules root)]
    (when (pos? depth)
      (def l (-> root-deps length dec))
      (eachp [i d] (sorted root-deps)
        (print-rule-tree
          rules d (dec depth)
          (string prefix-part (if (= i l) " └─" " ├─"))
          (string prefix-part (if (= i l) "   " " │ ")))))))

(defn- show-rule-tree
  [rules &opt root depth]
  (def max-depth (if depth (scan-number depth) math/inf))
  (if root
    (print-rule-tree rules root max-depth "" "")
    (let [ks (sort (seq [k :keys rules :when (string? k)] k))]
      (each k ks (print-rule-tree rules k max-depth "" "")))))

###
### Declare stubs
###

(defn declare-project
  ```
  Define your project metadata. This should
  be the first declaration in a project.janet file.
  Sets up the JPM-style build rule system as well as
  creates a number of default bundle hooks including:
  - build
  - clean
  - check
  - install
  - list-rules
  - rule-tree
  ```
  [&named name description url version repo tag dependencies]
  (assert name)
  (default dependencies @[])
  (def br (build-root))
  (def bd (build-dir))
  (def rules (get-rules))
  # Initialize build rules
  (build-rules/build-rule rules :install [])
  (build-rules/build-rule rules :pre-install [])
  (build-rules/build-rule rules :post-install [])
  (build-rules/build-rule rules :build [])
  (build-rules/build-rule rules :pre-build [])
  (build-rules/build-rule rules :post-build [])
  (build-rules/build-rule rules :check [])
  (build-rules/build-rule rules :pre-check [])
  (build-rules/build-rule rules :post-check [])
  (build-rules/build-rule rules :clean [])
  (build-rules/build-rule rules :pre-clean [])
  (build-rules/build-rule rules :post-clean [])
  # Add hooks
  (def e (curenv))
  (defn- prebuild
    []
    (os/mkdir br)
    (os/mkdir bd)
    (os/mkdir (path/join bd "static"))
    (build-rules/build-run e "pre-build" (dyn :workers)))
  (defn- postbuild
    []
    (build-rules/build-run e "post-build" (dyn :workers)))
  (defn- precheck
    []
    (build-rules/build-run e "pre-check" (dyn :workers)))
  (defn- postcheck
    []
    (build-rules/build-run e "post-check" (dyn :workers)))
  (defn- preinstall
    []
    (build-rules/build-run e "pre-install" (dyn :workers)))
  (defn- postinstall
    []
    (build-rules/build-run e "post-install" (dyn :workers)))
  (defn- preclean
    []
    (build-rules/build-run e "pre-clean" (dyn :workers)))
  (defn- postclean
    []
    (build-rules/build-run e "post-clean" (dyn :workers)))
  (defn build [&opt man target]
    (prebuild)
    (default target "build")
    (build-rules/build-run e target (dyn :workers))
    (postbuild))
  (defn install [manifest &]
    # (build) - removed since install in janet/src/boot/boot.janet calls build in the install hook
    (with-dyns [*install-manifest* manifest]
      (preinstall)
      (build-rules/build-run e "install" (dyn :workers))
      (postinstall)))
  (defn check [&]
    (build)
    (precheck)
    (run-tests)
    (postcheck))
  (defn list-rules [&]
    (each k (sorted (filter string? (keys rules)))
      (print k)))
  (defn rule-tree [&]
    (show-rule-tree rules))
  (defn clean [&]
    (preclean)
    (print "removing directory " bd)
    (sh/rm bd)
    (postclean))
  (defn clean-all [&]
    (preclean)
    (print "removing directory " br)
    (sh/rm br)
    (postclean))
  (defn run-task [task]
    (build-rules/build-run e task (dyn :workers)))
  (defglobal 'install install)
  (defglobal 'build build)
  (defglobal 'check check)
  (defglobal 'rule-tree rule-tree)
  (defglobal 'list-rules list-rules)
  (defglobal 'run run-task)
  (defglobal 'clean clean)
  (defglobal 'clean-all clean-all))

(defn declare-source
  "Create Janet modules. This does not actually build the module(s),
  but registers them for packaging and installation. :source should be an
  array of files and directores to copy into JANET_MODPATH or JANET_PATH.
  :prefix can optionally be given to modify the destination path to be
  (string JANET_PATH prefix source)."
  [&named source prefix]
  (defn dest [s] (def bn (path/basename s)) (if prefix (path/join prefix bn) bn))
  (def sources (if (bytes? source) [source] source))
  (when prefix
    (rule :pre-install []
          (def manifest (assert (dyn *install-manifest*)))
          (bundle/add-directory manifest prefix)))
  (each s source
    (install-rule s (dest s))))

(defn declare-headers
  "Declare headers for a library installation. Installed headers can be used by other native
  libraries."
  [&named headers prefix]
  (defn dest [s] (def bn (path/basename s)) (if prefix (path/join prefix bn) bn))
  (def headers (if (bytes? headers) [headers] headers))
  (when prefix
    (rule :pre-install []
          (def manifest (assert (dyn *install-manifest*)))
          (bundle/add-directory manifest prefix)))
  (each s headers
    (install-rule s (dest s))))

(defn declare-bin
  "Declare a generic file to be installed as an executable."
  [&named main]
  (install-rule main "bin" 8r755 (mkbin) true))

(defn declare-binscript
  ``Declare a janet file to be installed as an executable script. Creates
  a shim on windows. If hardcode is true, will insert code into the script
  such that it will run correctly even when JANET_PATH is changed. if auto-shebang
  is truthy, will also automatically insert a correct shebang line.
  ``
  [&named main hardcode-syspath is-janet]
  (def main (path/abspath main))
  (def dest (path/join "bin" (path/basename main)))
  (defn contents []
    (with [f (file/open main :rbn)]
      (def first-line (:read f :line))
      (def auto-shebang (and is-janet (not (string/has-prefix? "#!" first-line))))
      (def dynamic-syspath (= hardcode-syspath :dynamic))
      (def second-line (string/format "(put root-env :original-syspath (os/realpath (dyn *syspath*))) # auto generated\n"))
      (def third-line (string/format "(put root-env :syspath %v) # auto generated\n" (dyn *syspath*)))
      (def fourth-line (string/format "(put root-env :install-time-syspath %v) # auto generated\n" (dyn *syspath*)))
      (def last-line "\n(put root-env :syspath (get root-env :original-syspath)) # auto generated\n")
      (def rest (:read f :all))
      (string (if auto-shebang (string "#!/usr/bin/env janet\n"))
              first-line
              (if (or dynamic-syspath hardcode-syspath) second-line)
              (if hardcode-syspath third-line)
              (if hardcode-syspath fourth-line)
              rest
              (if dynamic-syspath last-line))))
  (install-buffer contents dest 8r755 (mkbin))
  (when (is-win-or-mingw)
    (def absdest (path/join (dyn *syspath*) dest))
    (def bat (string "@echo off\r\ngoto #_undefined_# 2>NUL || title %COMSPEC% & janet \"" absdest "\" %*"))
    (def newname (string main ".bat"))
    (install-buffer bat (string dest ".bat") nil (mkbin)))
  dest)

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
  (install-rule ipath iname)
  ipath)

(defn declare-manpage
  "Mark a manpage for installation. Manpages will be installed to $SYSPATH/man/man1/`(basename page)`"
  [page]
  (def page-name (path/basename page))
  (install-rule page (path/join "man" "man1" page-name) nil (mkman)))

(defn declare-documentation
  "Mark a file as general documentation to be installed. Documentation will be installed to $SYSYPATH/man/`prefix`/`(basename source)`"
  [&named source prefix]
  (default prefix "")
  (def page-name (path/basename source))
  (install-rule source (path/join "man" prefix page-name) nil (mkman)))

(defn declare-native
  "Declare a native module. This is a shared library that can be loaded
  dynamically by a janet runtime. This also builds a static libary that
  can be used to bundle janet code and native into a single executable."
  [&named name source embedded lflags libs cflags
   c++flags defines install nostatic static-libs
   use-rpath use-rdynamic pkg-config-flags dynamic-libs msvc-libs
   ldflags # alias for libs
   pkg-config-libs smart-libs c-std c++-std target-os]

  (def rules (get-rules))

  # Defaults
  (default libs ldflags)
  (default embedded @[])
  (default nostatic false)
  (default defines @{})
  (default smart-libs false)
  (default msvc-libs @[])
  (def toolchain (get-toolchain))

  (def msvc-libs @[;msvc-libs])
  (if (= :msvc toolchain)
    (array/push msvc-libs (cc/msvc-janet-import-lib)))

  # Create build environment table
  (def benv @{cc/*build-dir* (build-dir)
              cc/*defines* defines
              cc/*libs* libs
              cc/*msvc-libs* msvc-libs
              cc/*lflags* lflags
              cc/*cflags* cflags
              cc/*c++flags* c++flags
              cc/*static-libs* static-libs
              cc/*dynamic-libs* dynamic-libs
              cc/*smart-libs* smart-libs
              cc/*use-rdynamic* use-rdynamic
              cc/*use-rpath* use-rpath
              cc/*pkg-config-flags* pkg-config-flags
              cc/*cc* (toolchain-to-cc toolchain)
              cc/*c++* (toolchain-to-c++ toolchain)
              cc/*c-std* c-std
              cc/*c++-std* c++-std
              cc/*target-os* target-os
              cc/*visit* cc/visit-add-rule
              *toolchain* toolchain
              cc/*rules* rules})
  (table/setproto benv (curenv)) # configurable?
  (when (and pkg-config-libs (next pkg-config-libs))
    (with-env benv (cc/pkg-config ;pkg-config-libs))) # Add package config flags

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

  (when (= toolchain :msvc) # Disable import lib generation for natives
    (put benv cc/*lflags* @[;(get benv cc/*lflags* @[]) "/NOIMPLIB"]))

  (var has-cpp false)
  (each s source
    (case (path/ext s)
      ".cc" (set has-cpp true)
      ".cxx" (set has-cpp true)
      ".cpp" (set has-cpp true)))

  (def targets @{})
  (with-env benv
    (def to (cc/out-path name suffix))
    (put targets :native to)
    (install-rule to (string name suffix))
    (def objects @[])
    (loop [src :in embedded]
      (def c-src (cc/out-path src ".dynamic.c"))
      (def o-src (cc/out-path src ".dynamic.o"))
      (array/push objects o-src)
      (build-rules/build-rule rules c-src [src]
                              (create-buffer-c (slurp src) c-src (embed-name src)))
      (compile-c c-src o-src))
    (cc to ;source ;objects)
    (def ename (entry-name name))
    (build-rules/build-rule rules :build [to])
    (unless nostatic
      (def toa (cc/out-path name asuffix))
      (def tometa (cc/out-path name ".meta.janet"))
      (put targets :static toa)
      (install-rule toa (string name asuffix))
      (install-rule tometa (string name ".meta.janet"))
      (with-dyns [cc/*build-dir* (path/join (build-dir) "static")
                  cc/*defines* (merge-into @{"JANET_ENTRY_NAME" ename} defines)]
        (def sobjects @[])
        (loop [src :in embedded]
          (def c-src (cc/out-path src ".static.c"))
          (def o-src (cc/out-path src ".static.o"))
          (array/push sobjects o-src)
          (build-rules/build-rule rules c-src [src]
                                  (create-buffer-c (slurp src) c-src (embed-name src)))
          (compile-c c-src o-src))
        (static-cc toa ;source ;sobjects)
        (build-rules/build-rule
          rules tometa []
          (spit
            tometa
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
               :use-rpath use-rpath})))
        (build-rules/build-rule rules :build [toa tometa]))))

  targets)

###
### Quickbin functionality and declare-executable
###

(defn modpath-to-meta
  "Get the meta file path (.meta.janet) corresponding to a native module path (.so)."
  [toolchain path]
  (def suffix (case toolchain :msvc ".dll" ".so"))
  (string (string/slice path 0 (- (length suffix))) "meta.janet"))

(defn modpath-to-static
  "Get the static library (.a) path corresponding to a native module path (.so)."
  [toolchain path]
  (def suffix (case toolchain :msvc ".dll" ".so"))
  (def asuffix (case toolchain :msvc ".lib" ".a"))
  (string (string/slice path 0 (- -1 (length suffix))) asuffix))

(defn- make-bin-source
  [declarations lookup-into-invocations no-core]
  (string
    declarations
    ```
int main(int argc, const char **argv) {

#if defined(JANET_PRF)
    uint8_t hash_key[JANET_HASH_KEY_SIZE + 1];
#ifdef JANET_REDUCED_OS
    char *envvar = NULL;
#else
    char *envvar = getenv("JANET_HASHSEED");
#endif
    if (NULL != envvar) {
        strncpy((char *) hash_key, envvar, sizeof(hash_key) - 1);
    } else if (janet_cryptorand(hash_key, JANET_HASH_KEY_SIZE) != 0) {
        fputs("unable to initialize janet PRF hash function.\n", stderr);
        return 1;
    }
    janet_init_hash_key(hash_key);
#endif

    janet_init();

    ```
    (if no-core
      ```
    /* Get core env */
    JanetTable *env = janet_table(8);
    JanetTable *lookup = janet_core_lookup_table(NULL);
    JanetTable *temptab;
    int handle = janet_gclock();
    ```
      ```
    /* Get core env */
    JanetTable *env = janet_core_env(NULL);
    JanetTable *lookup = janet_env_lookup(env);
    JanetTable *temptab;
    int handle = janet_gclock();
    ```)
    lookup-into-invocations
    ```
    /* Unmarshal bytecode */
    Janet marsh_out = janet_unmarshal(
      janet_payload_image_embed,
      janet_payload_image_embed_size,
      0,
      lookup,
      NULL);

    /* Verify the marshalled object is a function */
    if (!janet_checktype(marsh_out, JANET_FUNCTION)) {
        fprintf(stderr, "invalid bytecode image - expected function.");
        return 1;
    }
    JanetFunction *jfunc = janet_unwrap_function(marsh_out);

    /* Check arity */
    janet_arity(argc, jfunc->def->min_arity, jfunc->def->max_arity);

    /* Collect command line arguments */
    JanetArray *args = janet_array(argc);
    for (int i = 0; i < argc; i++) {
        janet_array_push(args, janet_cstringv(argv[i]));
    }

    /* Create enviornment */
    temptab = env;
    janet_table_put(temptab, janet_ckeywordv("args"), janet_wrap_array(args));
    janet_table_put(temptab, janet_ckeywordv("executable"), janet_cstringv(argv[0]));
    janet_gcroot(janet_wrap_table(temptab));

    /* Unlock GC */
    janet_gcunlock(handle);

    /* Run everything */
    JanetFiber *fiber = janet_fiber(jfunc, 64, argc, argc ? args->data : NULL);
    fiber->env = temptab;
#ifdef JANET_EV
    janet_gcroot(janet_wrap_fiber(fiber));
    janet_schedule(fiber, janet_wrap_nil());
    janet_loop();
    int status = janet_fiber_status(fiber);
    janet_deinit();
    return status;
#else
    Janet out;
    JanetSignal result = janet_continue(fiber, janet_wrap_nil(), &out);
    if (result != JANET_SIGNAL_OK && result != JANET_SIGNAL_EVENT) {
      janet_stacktrace(fiber, out);
      janet_deinit();
      return result;
    }
    janet_deinit();
    return 0;
#endif
}

```))

(defn declare-executable
  "Declare a janet file to be the entry of a standalone executable program. The entry
  file is evaluated and a main function is looked for in the entry file. This function
  is marshalled into bytecode which is then embedded in a final executable for distribution."
  [&named name entry install headers no-compile no-core defines
   pkg-config-flags target-os deps static
   pkg-config-libs smart-libs c-std c++-std msvc-libs
   ldflags # alias for libs
   cflags c++flags lflags libs static-libs dynamic-libs use-rpath use-rdynamic]

  (default libs ldflags)
  (default libs @[])
  (default cflags @[])
  (default lflags @[])
  (default static-libs @[])
  (default dynamic-libs @[])
  (default headers @[])
  (default c++flags @[])
  (default defines @{})
  (default deps @[])
  (default pkg-config-libs @[])
  (default pkg-config-flags @[])
  (default msvc-libs @[])

  (assert (string? entry))
  (assert (string? name))

  (def name (if (is-win-or-mingw) (string name ".exe") name))
  (def bd (build-dir))
  (def dest (path/join bd name))
  (def cimage-dest (string dest ".c"))
  (when install (install-rule dest (path/join "bin" name) nil (mkbin) true))
  (def target (if no-compile cimage-dest dest))
  (rule :build [target])
  (rule target [entry ;headers ;deps]
        (print "generating executable c source " cimage-dest " from " entry "...")
        (sh/create-dirs-to dest)
        (flush)
        # Get compiler
        (def toolchain (get-toolchain))
        # Do entry
        (def module-cache @{})
        (def env (make-env))
        (put env *module-make-env* (fn :module-make-env [&opt e] (default e env) (make-env e)))
        (put env *module-cache* module-cache)
        (put env :build bd) # expose build directory to executable main (see test-bundle for example)
        (dofile entry :env env)
        (def main (module/value env 'main))
        (def dep-lflags @[])
        (def dep-libs @[])

        # Create marshalling dictionary
        (def mdict1 (invert (env-lookup root-env)))
        (def mdict
          (if no-core
            (let [temp @{}]
              (eachp [k v] mdict1
                (if (or (cfunction? k) (abstract? k))
                  (put temp k v)))
              temp)
            mdict1))

        # Load all native modules
        (def prefixes @{})
        (def static-libs @[])
        (loop [[name m] :pairs module-cache
               :let [n (m :native)]
               :when n
               :let [prefix (gensym)]]
          (print "found native " n "...")
          (flush)
          (put prefixes prefix n)
          (array/push static-libs (modpath-to-static toolchain n))
          (def oldproto (table/getproto m))
          (table/setproto m nil)
          (loop [[sym value] :pairs (env-lookup m)]
            (put mdict value (symbol prefix sym)))
          (table/setproto m oldproto))

        # Find static modules
        (var has-cpp false)
        (def declarations @"#include <janet.h>\n")
        (def lookup-into-invocations @"")
        (loop [[prefix name] :pairs prefixes]
          (def meta (eval-string (slurp (modpath-to-meta toolchain name))))
          (if (meta :cpp) (set has-cpp true))
          (buffer/push-string lookup-into-invocations
                              "    temptab = janet_table(0);\n"
                              "    temptab->proto = env;\n"
                              "    " (meta :static-entry) "(temptab);\n"
                              "    janet_env_lookup_into(lookup, temptab, \""
                              prefix
                              "\", 0);\n\n")
          (when-let [lfs (meta :lflags)]
            (array/concat dep-lflags lfs))
          (when-let [lfs (or (meta :libs) (meta :ldflags))]
            (array/concat dep-libs lfs))
          (buffer/push-string declarations
                              "extern void "
                              (meta :static-entry)
                              "(JanetTable *);\n"))

        # Build image
        (def image (marshal main mdict))
        (create-buffer-c image cimage-dest "janet_payload_image")
        (spit cimage-dest (make-bin-source declarations lookup-into-invocations no-core) :ab)

        # Extra command line options for building executable statically linked with libjanet
        (def msvc-libs-extra @[])
        (def other-cflags @[])
        (if (= toolchain :msvc)
          (do
            (def libpath (path/join (cc/get-msvc-prefix) "C"))
            (def msvc-libjanet (path/join libpath "libjanet.lib"))
            (def janeth (path/join libpath "janet.h"))
            (assert (= (os/stat janeth :mode) :file) "janet.h not found in expected location, possible misconfiguration")
            (assert (= (os/stat msvc-libjanet :mode) :file) "libjanet.lib not found in expected location, possible misconfiguration")
            (array/push other-cflags (string "/I" libpath))
            (array/push msvc-libs-extra msvc-libjanet))
          (do
            (def prefix (cc/get-unix-prefix))
            (def headerpath (path/join prefix "include"))
            (def libpath (path/join prefix "lib"))
            (def libjanet (path/join libpath "libjanet.a"))
            (def janeth (path/join headerpath "janet.h"))
            (assert (= (os/stat janeth :mode) :file) "janet.h not found in expected location, possible misconfiguration")
            (assert (= (os/stat libjanet :mode) :file) "libjanet.a not found in expected location, possible misconfiguration")
            (array/push other-cflags (string "-I" headerpath))
            (array/push dep-libs libjanet)))

        # Static compilation will not work out of the box on mac, so disable with warning.
        (def static
          (if (and (= (os/which) :macos) static)
            (do
              (eprint "Warning! fully static executable disabled on macos.")
              false)
            static))

        # locally specific distinct to deal with macOS flags like ["-framework" "Cocoa"]
        (defn distinct-flags [args]
          (def res @[])
          (def seen @{})
          (var item (next args))
          (while item
            (def val (args item))
            (if (or (= val "-framework") (= val "-weak_framework"))
              (do
                (set item (next args item))
                (def combined-flag (string val (args item)))
                (unless (in seen combined-flag)
                  (put seen combined-flag true)
                  (array/push res val (args item))))
              (unless (in seen val)
                (put seen val true)
                (array/push res val)))
            (set item (next args item)))
          res)

        (def benv @{cc/*build-dir* bd
                    cc/*defines* defines
                    cc/*libs* libs
                    cc/*msvc-libs* (distinct [;msvc-libs-extra ;msvc-libs ;static-libs])
                    cc/*lflags* (distinct-flags [;lflags ;dep-lflags])
                    cc/*cflags* [;other-cflags ;cflags]
                    cc/*c++flags* (distinct [;other-cflags ;c++flags])
                    cc/*static-libs* (distinct [;dep-libs ;static-libs])
                    cc/*smart-libs* smart-libs
                    cc/*use-rdynamic* use-rdynamic
                    cc/*use-rpath* use-rpath
                    cc/*pkg-config-flags* pkg-config-flags
                    cc/*c-std* c-std
                    cc/*c++-std* c++-std
                    cc/*cc* (toolchain-to-cc toolchain)
                    cc/*c++* (toolchain-to-c++ toolchain)
                    cc/*target-os* target-os
                    cc/*visit* cc/visit-execute-if-stale
                    cc/*rules* (get-rules)})
        (table/setproto benv (curenv)) # configurable?
        (when (and pkg-config-libs (next pkg-config-libs))
          (with-env benv (cc/pkg-config ;pkg-config-libs))) # Add package config flags
        (def compile-c
          (case toolchain
            :msvc cc/msvc-compile-c
            cc/compile-c))
        (def link
          (case toolchain
            :msvc cc/msvc-link-executable
            (if has-cpp cc/link-executable-c++ cc/link-executable-c)))
        (unless no-compile
          (with-env benv
            (def oimage-dest (cc/out-path cimage-dest ".o"))
            (cc/search-libraries "m" "rt" "dl")
            (flush)
            (compile-c cimage-dest oimage-dest)
            (flush)
            (link [oimage-dest] dest static))))
  target)

(defn quickbin
  ```
  Create an executable file from a script with a main function.
  ```
  [entry output]
  (def rules @{})
  (with-dyns [cc/*rules* rules
              *build-root* "_quickbin"]
    (defer (sh/rm "_quickbin")
      (declare-project :name "fake-project")
      (def target (declare-executable :entry entry :name output))
      (build-rules/build-run rules "build")
      (print "copying " target " to " output)
      (sh/copy target output))))

###
### Create an environment that emulates jpm's project.janet environment
###

(defmacro post-deps
  "Run code at the top level if jpm dependencies are installed. Build
  code that imports dependencies should be wrapped with this macro, as project.janet
  needs to be able to run successfully even without dependencies installed."
  [& body]
  (unless (dyn :jpm-no-deps)
    ~',(reduce |(eval $1) nil body)))

(def- declare-cc (curenv))
(def- sh (require "./sh"))
(def- cjanet (require "./cjanet"))
(def- cc (require "./cc"))
(def- path (require "./path"))
(defn jpm-shim-env
  "Create an environment table that can evaluate project.janet files"
  [&opt e]
  (default e (curenv))
  (pm-config/read-env-variables e)
  # TODO - one for one naming with jpm
  (merge-module e sh)
  (merge-module e cjanet)
  (merge-module e declare-cc)
  (merge-module e cc)
  (merge-module e path)
  # TODO - fake some other functions a bit better as well
  (put e 'default-cflags @{:value @[]})
  (put e 'default-lflags @{:value @[]})
  (put e 'default-ldflags @{:value @[]})
  (put e 'default-cppflags @{:value @[]})
  e)
