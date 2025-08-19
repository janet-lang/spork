(use ../spork/test)
(import ../spork/pm)
(import ../spork/pm-config)
(import ../spork/sh)
(import ../spork/path)

(start-suite)

(assert true) # smoke test

# Copy since not exposed in boot.janet
(defn- bundle-rpath
  [path]
  (string/replace-all "\\" "/" (os/realpath path)))

(defn randdir
  "Get a random directory name"
  []
  (string (os/cwd) "/tmp/tmp_dir_" (slice (string (math/random) ".tmp") 2)))

(defn count-match
  "Count number of occurrence of pat in str"
  [pat str]
  (length (string/find-all pat str)))

(defn after?
  "Verify pat2 occurs after pat1 in str"
  [pat2 pat1 str]
  (let [f1 (string/find pat1 str)
        f2 (string/find pat2 str)]
    (> f2 f1)))

(defn divider [title]
  (printf "------------------------------------------\n%s" title))

(defn dump-file [filename]
  (let [contents (slurp filename)]
    (printf "---------- %s -------------" filename)
    (var count 0)
    (loop [line :in (string/split "\n" contents)]
      (set count (inc count))
      (printf "%5d %s" count line))
    (print "----------------------------")))

(defn dump-out [output]
  (let [out (string/split "\n" (output :out))
        err (string/split "\n" (output :err))]
    (when (> (length out) 0)
      (each line out
        (printf line)))
    (when (> (length err) 0)
      (each line err
        (printf line)))))

(def bat (if (= (os/which) :windows) ".bat" ""))

# Create a temporary directory for our janet tree
(math/seedrandom (os/cryptorand 16))
(def syspath (randdir))
(sh/rm syspath)
(os/mkdir "tmp")
(assert (os/mkdir syspath))
(pm-config/read-env-variables root-env)
(put root-env :build-dir nil) # jpm test sets this and it messes things up
(defer (sh/rm "tmp")
       (put root-env *syspath* (bundle-rpath syspath))
       (put root-env :binpath (string syspath "/bin"))
       (put root-env :manpath (string syspath "/man"))
       (unless (os/getenv "VERBOSE")
         (setdyn *out* @""))
       (assert (empty? (bundle/list)) "initial bundle/list")
       (assert (empty? (bundle/topolist)) "initial bundle/topolist")

       # install spork
       (pm/pm-install "file::.")
       (assert (= 1 (length (bundle/list))) "bundle/list after install")

       # make sure the right janet-pm is found and
       # verify all bin scripts have two copies of hard-coded syspath, one in body
       # and one in main
       (def binpath (path/join (dyn *syspath*) "bin"))
       (def janet-pm (path/join binpath (string "janet-pm")))
       (assert (sh/exists? janet-pm))
       (dump-out (sh/exec-slurp-all (string janet-pm bat) "show-config"))
       (assert (= 1 (length (string/find-all "put root-env :install-time-syspath" (slurp janet-pm)))))

       # check sub commands work, even without defining pre-/post-

       (os/cd "test-bundle")
       (divider "Running janet-pm clean in a project that doesn't define pre-clean/post-clean")
       (def empty-out (sh/exec-slurp-all (string janet-pm bat) "clean"))
       (dump-out empty-out)
       (assert (= 0 (count-match "error" (empty-out :err))))

       # run janet-pm commands and collect output

       (os/cd "../test-project")

       # check "build"
       (divider "Running janet-pm build")
       (def build-out (sh/exec-slurp-all (string janet-pm bat) "build"))
       (dump-out build-out)
       (assert (= 1 (count-match "** pre-build" (build-out :out))))
       (assert (= 1 (count-match "** post-build" (build-out :out))))
       (assert (= 0 (count-match "** pre-install" (build-out :out))))

       # check "install"
       (divider "Running janet-pm install")
       (def install-out (sh/exec-slurp-all (string janet-pm bat) "install"))
       (dump-out install-out)
       # verify proper entries only occur once
       (assert (= 1 (count-match "** pre-build" (install-out :out))))
       (assert (= 1 (count-match "** post-build" (install-out :out))))
       (assert (= 1 (count-match "** pre-install" (install-out :out))))
       (assert (= 1 (count-match "** post-install" (install-out :out))))
       (assert (= 0 (count-match "** pre-check" (install-out :out))))
       (assert (= 0 (count-match "** post-check" (install-out :out))))

       # verify order of entries
       (assert (after? "post-build" "pre-build" (install-out :out)))
       (assert (after? "pre-install" "post-build" (install-out :out)))
       (assert (after? "post-install" "pre-install" (install-out :out)))

       # check that syspath is correctly set inside a main in a binscript
       (def bin-with-main (path/join binpath (string "bin-with-main")))
       (def bin-out (sh/exec-slurp-all (string bin-with-main bat)))
       (dump-out bin-out)
       (assert (string/find (path/basename (dyn *syspath*)) (bin-out :out)))

       # check "test"
       (divider "Running janet-pm test")
       (def test-out (sh/exec-slurp-all (string janet-pm bat) "test"))
       (dump-out test-out)
       # verify proper entries only occur once
       (assert (= 1 (count-match "** pre-build" (test-out :out))))
       (assert (= 1 (count-match "** post-build" (test-out :out))))
       (assert (= 0 (count-match "** pre-install" (test-out :out))))
       (assert (= 0 (count-match "** post-install" (test-out :out))))
       (assert (= 1 (count-match "** pre-check" (test-out :out))))
       (assert (= 1 (count-match "** post-check" (test-out :out))))
       (assert (= 1 (count-match "running test" (test-out :out))))

       # verify order of entries
       (assert (after? "post-build" "pre-build" (test-out :out)))
       (assert (after? "pre-check" "post-build" (test-out :out)))
       (assert (after? "running test" "pre-check" (test-out :out)))
       (assert (after? "post-check" "running test" (test-out :out)))

        # check "clean"
       (divider "Running janet-pm clean")
       (def clean-out (sh/exec-slurp-all (string janet-pm bat) "clean"))
       (dump-out clean-out)
       # verify proper entries only occur once
       (assert (= 1 (count-match "** pre-clean" (clean-out :out))))
       (assert (= 1 (count-match "removing directory _build" (clean-out :out))))
       (assert (= 1 (count-match "** post-clean" (clean-out :out))))
       (assert (= 0 (count-match "** pre-build" (clean-out :out))))
       (assert (= 0 (count-match "** post-build" (clean-out :out))))
       (assert (= 0 (count-match "** pre-install" (clean-out :out))))
       (assert (= 0 (count-match "** post-install" (clean-out :out))))
       (assert (= 0 (count-match "** pre-check" (clean-out :out))))
       (assert (= 0 (count-match "** post-check" (clean-out :out))))

       # verify order of entries
       (assert (after? "removing directory" "pre-clean" (clean-out :out)))
       (assert (after? "post-clean" "removing directory" (clean-out :out)))

        # check "clean-all"
       (divider "Running janet-pm clean-all")
       (def clean-all-out (sh/exec-slurp-all (string janet-pm bat) "clean-all"))
       (dump-out clean-all-out)
       # verify proper entries only occur once
       (assert (= 1 (count-match "** pre-clean" (clean-all-out :out))))
       (assert (= 1 (count-match "removing directory _build" (clean-all-out :out))))
       (assert (= 1 (count-match "** post-clean" (clean-all-out :out))))
       (assert (= 0 (count-match "** pre-build" (clean-all-out :out))))
       (assert (= 0 (count-match "** post-build" (clean-all-out :out))))
       (assert (= 0 (count-match "** pre-install" (clean-all-out :out))))
       (assert (= 0 (count-match "** post-install" (clean-all-out :out))))
       (assert (= 0 (count-match "** pre-check" (clean-all-out :out))))
       (assert (= 0 (count-match "** post-check" (clean-all-out :out))))

       # verify order of entries
       (assert (after? "removing directory" "pre-clean" (clean-all-out :out)))
       (assert (after? "post-clean" "removing directory" (clean-all-out :out))))

# reset *out* so we can see # tests passed even if VERBOSE is not defined
(setdyn *out* nil)

(end-suite)
