###
### Configuration from environment variables for pm.janet and declare-cc.janet.
###

# Fix for janet 1.35.2
(compwhen (not (dyn 'assertf))
  (defmacro- assertf
    "Convenience macro that combines `assert` and `string/format`."
    [x fmt & args]
    (def v (gensym))
    ~(do
       (def ,v ,x)
       (if ,v
         ,v
         (,errorf ,fmt ,;args)))))

(defn- set1
  [env d e &opt xform]
  (default xform identity)
  (when-let [x (os/getenv e)]
    (put env d (xform x))))

(defn- tobool
  [x]
  (get
    {"t" true "true" true "1" true "yes" true "on" true}
    (string/ascii-lower (string/trim x)) false))

(defn- toposint
  [x]
  (def y (scan-number x))
  (assertf (and (>= y 1) (int? y)) "expected a positive integer for number of workers, got %v" x)
  y)

(defn- make-enum
  [name & options]
  (def enum-set (tabseq [o :in options] o o))
  (fn enum
    [x]
    (def y (-> x string/ascii-lower keyword))
    (assertf (in enum-set y) "unknown option %v for %s. Expected one of %s." x name (string/join options ", "))
    y))

(def- build-type-xform (make-enum "build type" "debug" "develop" "release"))
(def- toochain-xform (make-enum "toolchain" "gcc" "clang" "msvc" "cc")) # TODO mingw, zig

(defn read-env-variables
  "Read and validate environment variables for configuration. These environment variables are
  translated to dynamic bindings and stored in an environment table. By default, store the bindings in the current environment."
  [&opt env]
  (default env (curenv))
  (when (get env :is-configured) (break))
  (set1 env :prefix "JANET_PREFIX")
  (set1 env :gitpath "JANET_GIT")
  (set1 env :curlpath "JANET_CURL")
  (set1 env :tarpath "JANET_TAR")
  (set1 env :msvc-cpath "JANET_LIBPATH")
  (set1 env :build-type "JANET_BUILD_TYPE" build-type-xform)
  (set1 env :toolchain "JANET_TOOLCHAIN" toochain-xform)
  (set1 env :build-dir "JANET_BUILD_DIR")
  (set1 env :offline "JANET_OFFLINE" tobool)
  (set1 env :pkglist "JANET_PKGLIST")
  (set1 env :workers "WORKERS" toposint)
  (set1 env :verbose "VERBOSE" tobool)
  (put env :is-configured true))
