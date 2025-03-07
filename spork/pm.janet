###
### Package management functionality. Augments janet's bundle/* module with git, tar, and
### curl support. Connects to a package registry as well, and provides some dependency management.
###
### Port of parts of jpm/pm.janet
###

(import ./sh)
(import ./build-rules)
(import ./path)

(defdyn *gitpath* "What git command to use to fetch dependencies")
(defdyn *tarpath* "What tar command to use to fetch dependencies")
(defdyn *curlpath* "What curl command to use to fetch dependencies")
(defdyn *pkglist* "Override the default package listing")

(def- filepath-replacer
  "Convert url with potential bad characters into a file path element."
  (peg/compile ~(% (any (+ (/ '(set "<>:\"/\\|?*") "_") '1)))))

(defn filepath-replace
  "Remove special characters from a string or path
  to make it into a path segment."
  [repo]
  (get (peg/match filepath-replacer repo) 0))

(defn git
  "Make a call to git."
  [& args]
  (sh/exec (dyn *gitpath* "git") ;args))

(defn tar
  "Make a call to tar."
  [& args]
  (sh/exec (dyn *tarpath* "tar") ;args))

(defn curl
  "Make a call to curl"
  [& args]
  (sh/exec (dyn *curlpath* "curl") ;args))

(defn- getpkglist []
  (dyn *pkglist* "https://github.com/janet-lang/pkgs.git"))

(var- bundle-install-recursive nil)

(defn- resolve-bundle-name
  "Convert short bundle names to full tables."
  [bname]
  (if-not (string/find ":" bname)
    (let [pkgs (try
                 (require "pkgs")
                 ([err]
                   (bundle-install-recursive (getpkglist))
                   (require "pkgs")))
          url (get-in pkgs ['packages :value (symbol bname)])]
      (unless url
        (error (string "bundle " bname " not found.")))
      url)
    bname))

(defn resolve-bundle
  "Convert any bundle string/table to the normalized table form."
  [bundle]
  (var repo nil)
  (var tag nil)
  (var btype :git)
  (var shallow false)
  (if (dictionary? bundle)
    (do
      (set repo (or (get bundle :url) (get bundle :repo)))
      (set tag (or (get bundle :tag) (get bundle :sha) (get bundle :commit) (get bundle :ref)))
      (set btype (get bundle :type :git))
      (set shallow (get bundle :shallow false)))
    (let [parts (string/split "::" bundle)]
      (case (length parts)
        1 (set repo (get parts 0))
        2 (do (set repo (get parts 1)) (set btype (keyword (get parts 0))))
        3 (do
            (set btype (keyword (get parts 0)))
            (set repo (get parts 1))
            (set tag (get parts 2)))
        (errorf "unable to parse bundle string %v" bundle))))
  {:url (resolve-bundle-name repo) :tag tag :type btype :shallow shallow})

(defn update-git-bundle
  "Fetch latest tag version from remote repository"
  [bundle-dir tag shallow]
  (if shallow
    (git "-C" bundle-dir "fetch" "--depth" "1" "origin" (or tag "HEAD"))
    (do
      # Tag can be a hash, e.g. in lockfile. Some Git servers don't allow
      # fetching arbitrary objects by hash. First fetch ensures, that we have
      # all objects locally.
      (git "-C" bundle-dir "fetch" "--tags" "origin")
      (git "-C" bundle-dir "fetch" "origin" (or tag "HEAD"))))
  (git "-C" bundle-dir "reset" "--hard" "FETCH_HEAD"))

(defn download-git-bundle
  "Download a git bundle from a remote respository"
  [bundle-dir url tag shallow]
  (var fresh false)
  (if (dyn :offline)
    (if (not= :directory (os/stat bundle-dir :mode))
      (error (string "did not find cached repository for dependency " url))
      (set fresh true))
    (when (os/mkdir bundle-dir)
      (set fresh true)
      (git "-c" "init.defaultBranch=master" "-C" bundle-dir "init")
      (git "-C" bundle-dir "remote" "add" "origin" url)
      (update-git-bundle bundle-dir tag shallow)))
  (unless (or (dyn :offline) fresh)
    (update-git-bundle bundle-dir tag shallow))
  (unless (dyn :offline)
    (git "-C" bundle-dir "submodule" "update" "--init" "--recursive")))

(defn download-tar-bundle
  "Download a dependency from a tape archive. The archive should have exactly one
  top level directory that contains the contents of the project."
  [bundle-dir url &opt force-gz]
  (def has-gz (string/has-suffix? "gz" url))
  (def is-remote (string/find ":" url))
  (def dest-archive (if is-remote (string bundle-dir "/bundle-archive." (if has-gz "tar.gz" "tar")) url))
  (os/mkdir bundle-dir)
  (when is-remote
    (curl "-sL" url "--output" dest-archive))
  (spit (string bundle-dir "/.bundle-tar-url") url)
  (def tar-flags (if has-gz "-xzf" "-xf"))
  (tar tar-flags dest-archive "--strip-components=1" "-C" bundle-dir))

(defn download-bundle
  "Download the package source (using git) to the local cache. Return the
  path to the downloaded or cached soure code."
  [url bundle-type &opt tag shallow]
  (def cache (path/join (dyn *syspath*) ".cache"))
  (os/mkdir cache)
  (def id (filepath-replace (string bundle-type "_" tag "_" url)))
  (def bundle-dir (string cache "/" id))
  (case bundle-type
    :git (download-git-bundle bundle-dir url tag shallow)
    :tar (download-tar-bundle bundle-dir url)
    (errorf "unknown bundle type %v" bundle-type))
  bundle-dir)

(defn- load-project-meta
  "Load the metadata from a project.janet file without doing a full evaluation
  of the project.janet file. Returns a struct with the project metadata. Raises
  an error if no metadata found."
  [&opt path]
  (default path "./project.janet")
  (def src (slurp path))
  (def p (parser/new))
  (parser/consume p src)
  (parser/eof p)
  (var ret nil)
  (while (parser/has-more p)
    (if ret (break))
    (def item (parser/produce p))
    (match item
      ['declare-project & rest] (set ret (table ;rest))))
  (unless ret
    (errorf "no metadata found in %s" path))
  ret)

(def- shimcode
````
(use spork/declare-cc)
(use spork/build-rules)
(def e (jpm-shim-env))
(dofile "project.janet" :env e)
(defn install [manifest &]
  (with-dyns [*install-manifest* manifest]
    (build-run e "install")))
(defn build [&] (build-run e "build"))
(defn check [&] (build-run e "test"))
(defn clean [&] (build-run e "clean"))
````)

(defn project-janet-shim
  "Add a bundle/ directory to a legacy jpm project directory to allow installation with janet --install. Adds spork
  as a dependency."
  [dir]
  (def project (path/join dir "project.janet"))
  (def bundle-hook-dir (path/join dir "bundle"))
  (def bundle-janet-path (path/join dir "bundle.janet"))
  (def bundle-init (path/join dir "bundle" "init.janet"))
  (def bundle-info (path/join dir "bundle" "info.jdn"))
  (if (os/stat bundle-hook-dir :mode) (break))
  (if (os/stat bundle-janet-path :mode) (break))
  (assert (os/stat project :mode) "did not find bundle directory, bundle.janet or project.janet")
  (def meta (load-project-meta project))
  (def deps (get meta :dependencies @[]))
  (unless (index-of "spork" deps) (array/push deps "spork"))
  (put meta :dependencies deps)
  (os/mkdir bundle-hook-dir)
  (spit bundle-init shimcode)
  (spit bundle-info (string/format "%j" meta))
  nil)

(defn pm-install
  "Install a bundle given a url, short name, or full 'bundle code'. The bundle source code will be fetched from
  git or a url, then installed with `bundle/install`."
  [bundle-code &opt force-update]
  (def bundle (resolve-bundle bundle-code))
  (def {:url url
        :tag tag
        :type bundle-type
        :shallow shallow}
    bundle)
  (unless force-update
    (var installed false)
    (each b (bundle/list)
      (def id (get (get (bundle/manifest b) :config {}) :url))
      (when (= id url)
        (set installed true)
        (break)))
    (if installed (break)))
  (def bdir (download-bundle url bundle-type tag shallow))
  (project-janet-shim bdir)
  '(when-with [f (file/open (path/join bdir "bundle" "info.jdn"))]
    (def info (parse (:read f :all)))
    (each dep (get info :dependencies @[])
      (pm-install dep)))
  (def config @{:pm-identifier bundle-code :url url :tag tag :type bundle-type :installed-with "spork/pm"})
  (bundle/install bdir :config config ;(kvs config)))

(set bundle-install-recursive pm-install)
