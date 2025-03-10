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
(defdyn *pkglist* "Override the default package listing if a `pkgs` bundle is not currently installed.")

(def default-pkglist
  "The default package listing for resolving short bundle names."
   "https://github.com/janet-lang/pkgs.git")

(def- filepath-replacer
  "Convert url with potential bad characters into a file path element."
  (peg/compile ~(% (any (+ (/ '(set "<>:\"/\\|?*") "_") '1)))))

(defn- filepath-replace
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
  (dyn *pkglist* default-pkglist))

(var- bundle-install-recursive nil)

(defn- resolve-bundle-name
  "Convert short bundle names to full tables."
  [bname]
  (if (or (string/has-prefix? "." bname) (string/find ":" bname))
    (break bname))
  (let [pkgs (try
               (require "pkgs")
               ([err]
                (bundle-install-recursive (getpkglist))
                (require "pkgs")))
        url (get-in pkgs ['packages :value (symbol bname)])]
    (unless url
      (error (string "bundle " bname " not found.")))
    url))

(defn resolve-bundle
  ```
  Convert any bundle string/table to the normalized table form. `bundle` can be any of the following forms:
 
  * A short name that indicates a package from the package listing.
  * A URL or path to a git repository
  * A URL or path to a .tar.gz archive
  * A string of 2 parts separated by "::" - {type}::{path-or-url}
  * A string of 3 parts separated by "::" - {type}::{path-or-url}::{tag}
  * A table or struct with the following keys:

  * `:url` or `:repo` - the URL or path of the git repository or of the .tar.gz file. Required.
  * `:tag`, `:sha`, `:commit`, or `:ref` - The revision to checkout from version control. Optional.
  * `:type` - The dependency type, either `:git`, `:tar`, or `:file`. The default is `:git`. Optional.
  * `:shallow` - If using a git dependency, clone the repository with `--depth=1`. Optional.
  ```
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
  {:url (if (= btype :file) (os/realpath repo) (resolve-bundle-name repo))
   :tag tag :type btype :shallow shallow})

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
  "Download a git bundle from a remote respository."
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

(defn- get-cachedir
  [url bundle-type tag]
  (def url (if (= tag :file) (os/realpath url) url)) # use absolute paths for file caches
  (def cache (path/join (dyn *syspath*) ".cache"))
  (os/mkdir cache)
  (def id (filepath-replace (string bundle-type "_" tag "_" url)))
  (path/join cache id))

(defn download-bundle
  "Download the package source (using git, curl+tar, or a file copy) to the local cache. Return the
  path to the downloaded or cached soure code."
  [url bundle-type &opt tag shallow]
  (def bundle-dir (get-cachedir url bundle-type tag))
  (case bundle-type
    :git (download-git-bundle bundle-dir url tag shallow)
    :tar (download-tar-bundle bundle-dir url)
    :file (sh/copy url bundle-dir)
    (errorf "unknown bundle type %v" bundle-type))
  bundle-dir)

(defn- slurp-maybe
  [path]
  (when-with [f (file/open path)]
    (def data (file/read f :all))
    data))

(defn load-project-meta
  "Load the metadata from a project.janet file without doing a full evaluation
  of the project.janet file. Returns a struct with the project metadata. Raises
  an error if no metadata found."
  [dir]
  # Check bundle paths first
  (def infopath (path/join dir "bundle" "info.jdn"))
  (def infopath2 (path/join dir "info.jdn"))
  (when-let [d (slurp-maybe infopath)] (break (parse d)))
  (when-let [d (slurp-maybe infopath2)] (break (parse d)))
  # Then check project.janet for declare-project
  (def path (path/join dir "project.janet"))
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
(defn build [&opt man & targets] (default targets "build") (build-run e targets))
(defn check [&] (build-run e "test"))
(defn clean [&] (build-run e "clean"))
````)

(defn jpm-dep-to-bundle-dep
  "Convert a remote dependency identifier to a bundle dependency name. `dep-name` is any value that can be passed to `pm-install`.
  Will return a string than can be passed to `bundle/reinstall`, `bundle/uninstall`, etc."
  [dep-name]
  (def {:url url
        :tag tag
        :type bundle-type}
    (resolve-bundle dep-name))
  (def key [url tag bundle-type])
  (var result nil)
  (each d (bundle/list)
    (def m (bundle/manifest d))
    (def check [(get m :url) (get m :tag) (get m :type)])
    (when (= check key)
      (set result (get m :name))
      (break)))
  result)

(defn bundle-dep-to-jpm-dep
  "Convert a bundle dependency name for an installed bundle to a remote dependency identifier than can be passed to `resolve-bundle`.
  Will raise an error if `bundle-name` is not installed, or if `bundle-name` was no installed via a remote identifier.
  Will return a string than can be passed to `bundle/reinstall`, `bundle/uninstall`, etc."
  [bundle-name]
  (def m (bundle/manifest bundle-name))
  (assertf (get m :url) "bundle %v was not installed with pm-install" bundle-name)
  (def code {:url (get m :url)
             :tag (get m :tag)
             :type (get m :type)
             :shallow true})
  code)

(defn- project-janet-shim
  ``If not already present, add a bundle/ directory to a legacy jpm project directory to allow installation with janet --install. Adds "spork"
  as a dependency. Return true if a default bundle/ directory was generated, false otherwise.``
  [dir]
  (def project (path/join dir "project.janet"))
  (def bundle-hook-dir (path/join dir "bundle"))
  (def bundle-janet-path (path/join dir "bundle.janet"))
  (def bundle-init (path/join dir "bundle" "init.janet"))
  (def bundle-info (path/join dir "bundle" "info.jdn"))
  (if (os/stat bundle-hook-dir :mode) (break false))
  (if (os/stat bundle-janet-path :mode) (break false))
  (assert (os/stat project :mode) "did not find bundle directory, bundle.janet or project.janet")
  (print "generating bundle/")
  (def meta (load-project-meta dir))
  (def deps (seq [d :in (get meta :dependencies @[])] d))
  (put meta :jpm-dependencies deps)
  (put meta :dependencies @["spork"])
  (os/mkdir bundle-hook-dir)
  (spit bundle-init shimcode)
  (spit bundle-info (string/format "%j" meta))
  true)

(defn pm-install
  "Install a bundle given a url, short name, or full 'bundle code'. The bundle source code will be fetched from
  git or a url, then installed with `bundle/install`."
  [bundle-code &opt no-deps force-update]
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
  (def did-shim (project-janet-shim bdir))
  (def info (load-project-meta bdir))
  (def jpm-deps (get info :jpm-dependencies @[]))
  (if-let [name (get info :name)]
    (if (bundle/installed? name) (break)))
  (unless no-deps
    (each dep jpm-deps
      (pm-install dep)))
  (when did-shim
    (def deps (seq [d :in jpm-deps] (jpm-dep-to-bundle-dep d)))
    (unless (index-of "spork" deps) (array/push deps "spork"))
    (put info :dependencies deps)
    (spit (path/join bdir "bundle" "info.jdn") (string/format "%j" info)))
  (def config @{:pm-identifier bundle-code :url url :tag tag :type bundle-type :installed-with "spork/pm"})
  (with-env root-env # work around for janet issue
    (bundle/install bdir :config config ;(kvs config))))

(defn local-hook
  "Run a bundle hook on the local project."
  [hook & args]
  (project-janet-shim ".")
  (def [ok module] (protect (require "/bundle")))
  (unless ok (break))
  (def hookf (module/value module (symbol hook)))
  (unless hookf (break))
  (hookf ;args))

(set bundle-install-recursive pm-install)
