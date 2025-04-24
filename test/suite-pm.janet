(use ../spork/test)
(import ../spork/pm)
(import ../spork/pm-config)
(import ../spork/sh)

(start-suite)

(assert true) # smoke test
(assert-docs "/spork/pm")

# Copy since not exposed in boot.janet
(defn- bundle-rpath
  [path]
  (string/replace-all "\\" "/" (os/realpath path)))

(defn randdir
  "Get a random directory name"
  []
  (string (os/cwd) "/tmp/tmp_dir_" (slice (string (math/random) ".tmp") 2)))

# Create a temporary directory for our janet tree
(math/seedrandom (os/cryptorand 16))
(def syspath (randdir))
(sh/rm syspath)
(os/mkdir "tmp")
(assert (os/mkdir syspath))
(pm-config/read-env-variables root-env)
(setdyn :build-dir nil) # jpm test sets this and it messes things up
(defer (sh/rm "tmp")
  (put root-env *syspath* (bundle-rpath syspath))
  (put root-env :binpath (string syspath "/bin"))
  (put root-env :manpath (string syspath "/man"))
  (unless (os/getenv "VERBOSE")
    (setdyn *out* @""))
  (assert (empty? (bundle/list)) "initial bundle/list")
  (assert (empty? (bundle/topolist)) "initial bundle/topolist")
  (sh/rm "./test-bundle/bundle")
  (sh/rm "./test-bundle/build")
  (sh/rm "./test-bundle/_build")

  # Check our project.janet based bundle
  (pm/pm-install "file::.") # install spork
  (pm/pm-install "file::./test-bundle")
  (assert (= 2 (length (bundle/list))) "bundle/list after install"))

(end-suite)
