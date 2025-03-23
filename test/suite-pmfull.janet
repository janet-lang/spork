(use ../spork/test)
(import ../spork/pm)
(import ../spork/pm-config)
(import ../spork/sh)

(start-suite)

(def enabled (= "1" (os/getenv "SPORK_TEST_ALL_PACKAGES")))

# Copy since not exposed in boot.janet
(defn- bundle-rpath
  [path]
  (string/replace-all "\\" "/" (os/realpath path)))

(defn randdir
  "Get a random directory name"
  []
  (string "/tmp/tmp_dir_" (slice (string (math/random) ".tmp") 2)))

(unless enabled (print "set SPORK_TEST_ALL_PACKAGES=1 to run full pm testing."))
(when enabled
  (assert true) # smoke test
  (assert-docs "spork/pm")
  (math/seedrandom (os/cryptorand 16))
  (def syspath (randdir))
  (sh/rm syspath)
  (os/mkdir "tmp")
  (assert (os/mkdir syspath))
  (pm-config/read-env-variables root-env)
  (put root-env *syspath* (bundle-rpath syspath))
  (put root-env :binpath (string (dyn *syspath*) "/bin"))
  (put root-env :manpath (string (dyn *syspath*) "/man"))
  (defer (sh/rm syspath)
    (unless (os/getenv "VERBOSE")
      (setdyn *out* @""))
    (assert (empty? (bundle/list)) "initial bundle/list")
    (assert (empty? (bundle/topolist)) "initial bundle/topolist")
    (pm/pm-install "file::.") # install spork
    (pm/pm-install "file::./test-bundle")
    (pm/pm-install "pkgs")
    (pm/pm-install "circlet")
    (pm/pm-install "joy")
    (pm/pm-install "sqlite3")
    (pm/pm-install "jhydro")))

(end-suite)
