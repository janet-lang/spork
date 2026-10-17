(use spork/test)
(import spork/pm)
(import spork/pm-config)
(import spork/sh)
(import spork/path)

(start-suite)

(assert true) # smoke test

# Copy since not exposed in boot.janet
(defn- bundle-rpath
  [path]
  (string/replace-all "\\" "/" (os/realpath path)))

(defn randdir
  "Get a random directory name"
  []
  (string (os/cwd) "/tmp/tag-man/tmp_dir_" (slice (string (math/random) ".tmp") 2)))

(defn count-match
  "Count number of occurrence of pat in str"
  [pat str]
  (length (string/find-all pat str)))

(defn divider [title]
  (printf "------------------------------------------\n%s" title))

(defn dump-out [output]
  (let [out (string/split "\n" (output :out))
        err (string/split "\n" (output :err))]
    (when (> (length out) 0)
      (each line out
        (print line)))
    (when (> (length err) 0)
      (each line err
        (print line)))))

(def bat (if (= (os/which) :windows) ".bat" ""))

(defn- write-default-bundle-init []
  (spit "./bundle/init.janet"
        ```(if (dyn :install-time-syspath)
             (use @install-time-syspath/spork/declare-cc)
             (use spork/declare-cc))
           (dofile "project.janet" :env (jpm-shim-env))```))

(defn- write-custom-bundle-init []
  (spit "./bundle/init.janet"
        ```
(if (dyn :install-time-syspath)
   (use @install-time-syspath/spork/declare-cc)
   (use spork/declare-cc))
(dofile "project.janet" :env (jpm-shim-env))
(task "fix-up-tag" []
  (when-let [man (dyn *install-manifest*)]
        (set (man :tag) "test-custom-tag")))

# attach this task to the post-install hook
(task "post-install" ["fix-up-tag"])
```))

(defn clean-bundle []
  (sh/rm-readonly "./bundle")
  (sh/rm-readonly "./build")
  (sh/rm-readonly "./_build"))

# Create a temporary directory for our janet tree
(math/seedrandom (os/cryptorand 16))
(def syspath (randdir))
(sh/rm-readonly syspath)
(os/mkdir "tmp")
(os/mkdir "tmp/tag-man")
(assert (os/mkdir syspath))
(pm-config/read-env-variables root-env)
(put root-env :build-dir nil) # jpm test sets this and it messes things up
(defer (sh/rm-readonly "tmp/tag-man")
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

  # should find :tag for spork
  (def spork-tag ((bundle/manifest "spork") :tag))
  (assert spork-tag "spork tag")
  (assert (>= (length spork-tag) 7) "spork tag length")
  
  # invoking janet-pm via the shell needs JANET_PATH set to
  # know about our custom syspath
  (os/setenv "JANET_PATH" (dyn *syspath*))

  # make sure the right janet-pm is found and
  (def binpath (path/join (dyn *syspath*) "bin"))
  (def janet-pm (path/join binpath (string "janet-pm")))
  (assert (sh/exists? janet-pm) "janet-pm exists in binpath")

  (os/cd "test-bundle")
  (clean-bundle)
  
  (divider "Running janet-pm install with the default")
  (let [empty-out (sh/exec-slurp-all (string janet-pm bat) "install")
        man (bundle/manifest "testmod")
        tag (get man :tag)]
    (dump-out empty-out)
    (assert (= 0 (count-match "error" (empty-out :err))))
    (assert tag)
    (assert (>= (length tag) 7))
    (assert (not (man :version)) "no :version in default manifest")
    # testmod should have same tag as spork since the are in the same repo
    (assert (= tag spork-tag)) "spork and test-bundle tags should match")

  # clean up any remaining bits from previous runs
  (bundle/uninstall "testmod")
  (clean-bundle)

  # create a new info.jdn with a user :version and :tag
  (os/mkdir "./bundle")
  (spit "./bundle/info.jdn" ``{:name "testmod" :tag "FOO" :version "1.2.3"}``)
  (write-default-bundle-init)
  
  (divider "Running janet-pm install with :version and :tag in bundle/info.jdn")
  (let [empty-out (sh/exec-slurp-all (string janet-pm bat) "install")
        man (bundle/manifest "testmod")]
    (dump-out empty-out)
    (assert (= 0 (count-match "error" (empty-out :err))))
    (assert (= "FOO" (get-in man [:info :tag])))
    (assert (= "1.2.3" (get-in man [:info :version])))
    # should NOT be a :tag in manifest if a user-provided one exists in :info
    (assert (nil? (get man :tag)) "no :tag in manifest due to :tag in info"))

  # clean up any remaining bits from previous runs
  (bundle/uninstall "testmod")
  (clean-bundle)

  # create a new info.jdn with a user :version and :tag
  (os/mkdir "./bundle")
  (spit "./bundle/info.jdn" ``{:name "testmod" :tag "BAR" :version "1.2.3"}``)
  (write-custom-bundle-init)

  (divider "Running janet-pm install with custom :version and :tag and a post-install step to add a :tag ")
  (let [empty-out (sh/exec-slurp-all (string janet-pm bat) "install")
        man (bundle/manifest "testmod")]
    (dump-out empty-out)
    (assert (= 0 (count-match "error" (empty-out :err))))
    (assert (= "BAR" (get-in man [:info :tag])))
    (assert (= "1.2.3" (get-in man [:info :version])))
    # should now be a custom :tag in manifest generated from our post-install step
    (assert (= "test-custom-tag" (get man :tag)) "custom tag from post-install step"))

  (bundle/uninstall "testmod")
  (clean-bundle)
  (os/cd "..") # leave test-bundle dir

  # create a new project, and install it
  (divider "Running janet-pm on a scaffolded new-project")
  (def proj-name "a-project")

  # this is the same as "janet-pm new-project a-project"
  (pm/scaffold-project proj-name {:c false :author "test" :description "description"})
  (assert (sh/exists? proj-name))

  # from here, we want git to fail to see a local directory as part of a git repo
  # we've created this in a tmp directory under "spork/test" so git will try
  # to use the "spork/.git" directory. 
  #
  # running just "git init" will prevent the tag code from seeing the spork
  # tag but there won't be a real tag to use so we should not see one.

  (defer (do (os/cd "..") (sh/rm-readonly proj-name))
    (os/cd proj-name)
    (sh/exec "git" "init" "--initial=branch=test")
    (let [empty-out (sh/exec-slurp-all (string janet-pm bat) "install")
          man (bundle/manifest proj-name)]
      (dump-out empty-out)
      (assert (= 0 (count-match "error" (empty-out :err))))
      (assert (nil? (get man :tag)) "should not have a tag with no files committed to git")
      (assert (= "0.0.0" (get-in man [:info :version])))))

  (divider "Running janet-pm on a scaffolded new-c-project")

  # this time, we create a new git repo and commit everything, so we should get
  # a :tag in the manifest
  # this is like running "janet-pm new-c-project a-project"
  (pm/scaffold-project proj-name {:c true :author "test" :description "description"})
  (assert (sh/exists? proj-name))

  (defer (do (os/cd "..") (sh/rm-readonly proj-name))
    (os/cd proj-name)
    # these first two only set the config for this temp repo
    (sh/exec "git" "init" "--initial-branch=test")
    (sh/exec "git" "config" "user.email" "you@example.com")
    (sh/exec "git" "config" "user.name" "Your Name")
    (sh/exec "git" "config" "commit.gpgsign" "false")
    (sh/exec "git" "add" ".")
    (sh/exec "git" "commit" "-m" "message" "-q")
    (let [empty-out (sh/exec-slurp-all (string janet-pm bat) "install")
          man (bundle/manifest proj-name)]
      (dump-out empty-out)
      (assert (= 0 (count-match "error" (empty-out :err))))
      (assert (not (nil? (get man :tag))) "should have a :tag after committing files to git")
      (assert (>= (length (man :tag)) 7))
      (assert (= "0.0.0" (get-in man [:info :version])))))
  )


# reset *out* so we can see # tests passed even if VERBOSE is not defined
(setdyn *out* nil)

(end-suite)
