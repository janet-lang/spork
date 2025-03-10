(use ../spork/test)
(import ../spork/pm)
(import ../spork/sh)

(start-suite)

(assert true) # smoke test
(assert-docs "spork/pm")

# Copy since not exposed in boot.janet
(defn- bundle-rpath
  [path]
  (string/replace-all "\\" "/" (os/realpath path)))

(defn randdir
  "Get a random directory name"
  []
  (string "/tmp/tmp_dir_" (slice (string (math/random) ".tmp") 2)))

# Create a temporary directory for our janet tree
(math/seedrandom (os/cryptorand 16))
(def syspath (randdir))
(sh/rm syspath)
(assert (os/mkdir syspath))
(defer (sh/rm syspath)
  (put root-env *syspath* (bundle-rpath syspath))
  (unless (os/getenv "VERBOSE")
    (setdyn *out* @""))
  (assert (empty? (bundle/list)) "initial bundle/list")
  (assert (empty? (bundle/topolist)) "initial bundle/topolist")

  # Check our project.janet based bundle
  (pm/pm-install "file::.") # install spork
  (pm/pm-install "file::./test-bundle")
  (assert (= 2 (length (bundle/list))) "bundle/list after install"))

(end-suite)
