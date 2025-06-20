###
### Package management functionality. Augments janet's bundle/* module with git, tar, and
### curl support. Connects to a package registry as well, and provides some dependency management.
###
### Port of parts of jpm/pm.janet
###
### This module is purposefully decoupled from declare-cc.

(import ./sh)
(import ./path)
(import ./pm-config)

(defdyn *gitpath* "What git command to use to fetch dependencies")
(defdyn *tarpath* "What tar command to use to fetch dependencies")
(defdyn *curlpath* "What curl command to use to fetch dependencies")
(defdyn *pkglist* "Override the default package listing if a `pkgs` bundle is not currently installed.")

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
  (dyn *pkglist* pm-config/default-pkglist))

(var- bundle-install-recursive nil)

(defn- resolve-bundle-name
  "Convert short bundle names to full tables."
  [bname]
  (if (string/find ":" bname) (break bname))
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
  ```
  [bundle]
  (var repo nil)
  (var tag nil)
  (var btype :git)
  (if (dictionary? bundle)
    (do
      (set repo (or (get bundle :url) (get bundle :repo)))
      (set tag (or (get bundle :tag) (get bundle :sha) (get bundle :commit) (get bundle :ref)))
      (set btype (get bundle :type :git)))
    (let [parts (string/split "::" bundle)]
      (case (length parts)
        1 (set repo (get parts 0))
        2 (do (set repo (get parts 1)) (set btype (keyword (get parts 0))))
        3 (do
            (set btype (keyword (get parts 0)))
            (set repo (get parts 1))
            (set tag (get parts 2)))
        (errorf "unable to parse bundle string %v" bundle))))
  (set repo (if (= btype :file) (os/realpath repo) (resolve-bundle-name repo)))
  (when (string/has-prefix? "git+" repo)
    (set repo (string/slice repo 4 -1))
    (assert (= :git btype)))
  {:url repo :tag tag :type btype})

(defn update-git-bundle
  "Fetch latest tag version from remote repository"
  [bundle-dir tag]
  # Tag can be a hash, e.g. in lockfile. Some Git servers don't allow
  # fetching arbitrary objects by hash. First fetch ensures that we have
  # all objects locally.
  (git "-C" bundle-dir "fetch" "--tags" "origin")
  (git "-C" bundle-dir "fetch" "--depth" "1" "origin" (or tag "HEAD"))
  (git "-C" bundle-dir "reset" "--hard" "FETCH_HEAD"))

(defn download-git-bundle
  "Download a git bundle from a remote respository."
  [bundle-dir url tag]
  (var fresh false)
  (if (dyn :offline)
    (if (not= :directory (os/stat bundle-dir :mode))
      (error (string "did not find cached repository for dependency " url))
      (set fresh true))
    (when (os/mkdir bundle-dir)
      (set fresh true)
      (git "-c" "init.defaultBranch=master" "-C" bundle-dir "init")
      (git "-C" bundle-dir "remote" "add" "origin" url)
      (update-git-bundle bundle-dir tag)))
  (unless (or (dyn :offline) fresh)
    (update-git-bundle bundle-dir tag))
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
  [url bundle-type &opt tag]
  (var bundle-dir (get-cachedir url bundle-type tag))
  (case bundle-type
    :git (download-git-bundle bundle-dir url tag)
    :tar (download-tar-bundle bundle-dir url)
    :file (set bundle-dir url)
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
(if (dyn :install-time-syspath)
  (use @install-time-syspath/spork/declare-cc)
  (use spork/declare-cc))
(dofile "project.janet" :env (jpm-shim-env))
````)

(defn- manifest-pm-extract
  "Extract the package manager source of a manifest. Needs to handle both pm and default janet installs."
  [m]
  (or
    (get m :pm) # installed by pm, has extra info like git repo, etc.
    (table/to-struct
      (merge-into @{:type :file :url (get m :local-source)} (get m :info {}))))) # just a path on disk, native janet support

(defn- bundle-name-to-bundle
  "Convert an installed bundle name to a pm bundle. Also handles bundles not installed with pm for debugging purposes."
  [bundle-name]
  (manifest-pm-extract (bundle/manifest bundle-name)))

(defn- name-lookup
  "Find the bundle name of a bundle address"
  [bundle-addr]
  (def {:url url
        :tag tag
        :type bundle-type} bundle-addr)
  (def key [url tag bundle-type])
  (var result nil)
  (each d (bundle/list)
    (def m (bundle/manifest d))
    (when m
      (def pm (manifest-pm-extract m))
      (def check [(get pm :url) (get pm :tag) (get pm :type)])
      (when (= check key)
        (set result (get m :name))
        (break))))
  result)

(defn jpm-dep-to-bundle-dep
  "Convert a remote dependency identifier to a bundle dependency name. `dep-name` is any value that can be passed to `pm-install`.
  Will return a string than can be passed to `bundle/reinstall`, `bundle/uninstall`, etc."
  [dep-name]
  (def bundle-name (name-lookup (resolve-bundle dep-name)))
  (unless bundle-name
    (eprintf "unable to resolve jpm style dependency %q to a local bundle" dep-name))
  bundle-name)

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
  (printf "generating %s" bundle-hook-dir)
  (def meta (load-project-meta dir))
  (def deps (seq [d :in (get meta :dependencies @[])] d))
  (put meta :jpm-dependencies deps)
  (put meta :dependencies @["spork"])
  (os/mkdir bundle-hook-dir)
  (spit bundle-init shimcode)
  (spit bundle-info (string/format "%j" meta))
  true)

(defn- dyn-env
  []
  (def e (make-env))
  (defn- add1
    [x]
    (eachp [k v] x
      (if (keyword? k)
        (put e k v)))
    (if-let [p (getproto x)]
      (add1 p)))
  (add1 (curenv))
  e)

(defn pm-install
  "Install a bundle given a url, short name, or full 'bundle code'. The bundle source code will be fetched from
  git or a url, then installed with `bundle/install`."
  [bundle-code &named no-deps force-update no-install auto-remove]
  (def bundle (resolve-bundle bundle-code))
  (def name (name-lookup bundle))
  (if (and name (not force-update)) (break))
  (def {:url url :type bundle-type :tag tag} bundle)
  (def bdir (download-bundle url bundle-type tag))
  (def did-shim (project-janet-shim bdir))
  (def info (load-project-meta bdir))
  (def infoname (get info :name))
  (when (and (not name) (bundle/installed? infoname))
    (def existing (bundle-name-to-bundle infoname))
    (eprintf "a conflicting bundle %v is already installed, keeping that one." infoname)
    (eprintf "  existing bundle: %.99M" existing)
    (eprintf "  skipped bundle   %.99M" bundle)
    (break))
  (def jpm-deps (get info :jpm-dependencies @[]))
  (unless no-deps
    (each dep jpm-deps
      (pm-install dep :force-update force-update :auto-remove true)))
  (when did-shim
    # patch deps after installing all jpm dependencies. This allows the bundle/* module to track dependencies, and
    # prevent things like uninstalling a dependency, breaking another installed package.
    (def deps (seq [d :in jpm-deps] (jpm-dep-to-bundle-dep d)))
    (def deps (filter identity deps))
    (unless (index-of "spork" deps)
      # if spork is not installed, we are installing to a different tree.
      (when (bundle/installed? "spork") (array/push deps "spork")))
    (put info :dependencies deps)
    (spit (path/join bdir "bundle" "info.jdn") (string/format "%.99m\n" info)))
  (def config @{:pm bundle :installed-with "spork/pm" :auto-remove auto-remove})
  (unless no-install
    (with-env (dyn-env) # work around bundle/* quirk with accidentally injecting hooks.
      (if (and name (bundle/installed? name))
        (bundle/reinstall name :config config ;(kvs config))
        (bundle/install bdir :config config ;(kvs config))))))

(defn local-hook
  "Run a bundle hook on the local project."
  [hook & args]
  (project-janet-shim ".")
  (def [ok module] (protect (require "/bundle")))
  (unless ok (break))
  (def hookf (module/value module (symbol hook)))
  (unless hookf (break))
  (hookf ;args))

###
### Lock files
###

(defn save-lockfile
  "Create a lockfile that can be used to reinstall all currently installed bundles at a later date."
  [lock-dest &opt allow-local]
  (def lock @[])
  (each b (bundle/topolist)
    (def manifest (bundle/manifest b))
    (def config (get manifest :config))
    (def pm (get manifest :pm (get config :pm {:type :file :url (get manifest :local-source)})))
    (def name (get manifest :name))
    (array/push lock {:name name :pm pm :config config}))
  (def buf @"[\n")
  (each d lock
    (string/format "%j" d) # check JDN correctness
    (buffer/format buf "%.99m\n" d))
  (buffer/push buf "]\n")
  (spit lock-dest buf)
  lock)

(defn load-lockfile
  "Install all saved dependencies in a lockfile."
  [lock-src]
  (def lock (-> lock-src slurp parse))
  (each d lock
    (def {:pm pm :name name} d)
    (pm-install pm true true)
    (assert (bundle/installed? name))))

(set bundle-install-recursive pm-install)

###
### Configuration via environment variables
###

###
### Project scaffolding
###
### Generate new projects quickly, ported from jpm
###

(def- template-peg
  "Extract string pieces to generate a templating function"
  (peg/compile
    ~{:sub (group
             (+ (* "${" '(to "}") "}")
                (* "$" '(some (range "az" "AZ" "09" "__" "--")))))
      :main (any (* '(to (+ "$$" -1 :sub)) (+ (/ '"$$" "$") :sub 0)))}))

(defn- make-template
  "Make a simple string template as defined by Python PEP292 (shell-like $ substitution).
  Also allows dashes in indentifiers."
  [source]
  (def frags (peg/match template-peg source))
  (def partitions (partition-by type frags))
  (def string-args @[])
  (each chunk partitions
    (case (type (get chunk 0))
      :string (array/push string-args (string ;chunk))
      :array (each sym chunk
               (array/push string-args ~(,get opts ,(keyword (first sym)))))))
  ~(fn [opts] (,string ,;string-args)))

(defmacro deftemplate
  ```
  Define a inline template as defined by Python PEP292 (shell-like $ substitution),
  and also allows dashes in indentifiers.

  It defines new function `template-name` that takes a dictionary `opts` containing
  substitutions as an argument. Keys should be keywords with the same name (sans :)
  as the substitution keys.

  Template is parsed from the last element of `body` which should be string and can contain substitutions.
  ```
  [template-name & body]
  ~(def ,template-name ,;(slice body 0 -2) ,(make-template (last body))))

(defn opt-ask
  ```
  Ask user for the value of the `key`. `input-options` should be a table with default values.
  If the default value for the `key` is not `nil`, it will return that.
  ```
  [key input-options]
  (def dflt (get input-options key))
  (if (nil? dflt)
    (string/trim (getline (string key "? ")))
    dflt))

(deftemplate project-template
  :private
  ````
  (declare-project
    :name "$name"
    :description ```$description ```
    :author ```$author ```
    :dependencies @["spork"]
    :version "0.0.0")

  (declare-source
    :source ["$name"])
  ````)

(deftemplate native-project-template
  :private
  ````
  (declare-project
    :name "$name"
    :description ```$description ```
    :author ```$author ```
    :dependencies @["spork"]
    :version "0.0.0")

  (declare-source
    :source ["$name"])

  (declare-native
    :name "${name}-native"
    :source @["c/module.c"])
  ````)

(deftemplate module-c-template
  :private
  ```
  #include <janet.h>

  /***************/
  /* C Functions */
  /***************/

  JANET_FN(cfun_hello_native,
           "($name/hello-native)",
           "Evaluate to \"Hello!\". but implemented in C.") {
      janet_fixarity(argc, 0);
      (void) argv;
      return janet_cstringv("Hello!");
  }

  /****************/
  /* Module Entry */
  /****************/

  JANET_MODULE_ENTRY(JanetTable *env) {
      JanetRegExt cfuns[] = {
          JANET_REG("hello-native", cfun_hello_native),
          JANET_REG_END
      };
      janet_cfuns_ext(env, "$name", cfuns);
  }
  ```)

(deftemplate exe-project-template
  :private
  ````
  (declare-project
    :name "$name"
    :description ```$description ```
    :author ```$author ```
    :dependencies @["spork"]
    :version "0.0.0")

  (declare-executable
    :name "$name"
    :entry "$name/init.janet")
  ````)

(deftemplate readme-template
  :private
  ```
  # ${name}

  Add project description here.
  ```)

(deftemplate changelog-template
  :private
  ```
  # Changelog
  All notable changes to this project will be documented in this file.
  Format for entries is <version-string> - release date.

  ## 0.0.0 - $date
  - Created this project.
  ```)

(deftemplate license-template
  :private
  ```
  Copyright (c) $year $author and contributors

  Permission is hereby granted, free of charge, to any person obtaining a copy of
  this software and associated documentation files (the "Software"), to deal in
  the Software without restriction, including without limitation the rights to
  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
  of the Software, and to permit persons to whom the Software is furnished to do
  so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
  ```)

(deftemplate init-template
  :private
  ```
  (defn hello
    `Evaluates to "Hello!"`
    []
    "Hello!")

  (defn main
    [& args]
    (print (hello)))
  ```)

(deftemplate test-template
  :private
  ```
  (use ../$name/init)

  (assert (= (hello) "Hello!"))
  ```)

(deftemplate native-test-template
  :private
  ```
  (use ${name}-native)

  (assert (= (hello-native) "Hello!"))
  ```)

(deftemplate bundle-init-template
  :private
  ````
  (defn install
    [manifest &]
    (bundle/add manifest "$name"))

  (defn build
    [&]
    (print "Nothing to build!"))

  (defn clean
    [&]
    (print "Nothing to clean!"))

  (defn check
    [&]
    (var pass-count 0)
    (var total-count 0)
    (def failing @[])
    (each dir (sorted (os/dir "test"))
      (def path (string "test/" dir))
      (when (string/has-suffix? ".janet" path)
        (def pass (zero? (os/execute [(dyn *executable* "janet") "--" path] :p)))
        (++ total-count)
        (unless pass (array/push failing path))
        (when pass (++ pass-count))))
    (if (= pass-count total-count)
      (print "All tests passed!")
      (do
        (printf "%d of %d passed." pass-count total-count)
        (print "failing scripts:")
        (each f failing
          (print "  " f))
        (os/exit 1))))
  ````)

(deftemplate bundle-info-template
  :private
  ````
  {:name "$name"
   :description ```$description ```
   :author ```$author ```
   :dependencies @[]
   :version "0.0.0"}
  ````)

(defn- format-date
  []
  (def x (os/date))
  (string/format "%d-%.2d-%.2d" (x :year) (inc (x :month)) (inc (x :month-day))))

(defn scaffold-project
  "Generate a standardized project scaffold."
  [name &opt options]
  (default options {})
  (def year (get (os/date) :year))
  (def author (opt-ask :author options))
  (def description (opt-ask :description options))
  (def date (format-date))
  (def scaffold-native (get options :c))
  (def scaffold-exe (get options :exe))
  (def scaffold-spork-free (get options :no-spork))
  (def template-opts (merge-into @{:name name :year year :author author :date date :description description} options))
  (print "creating project directory for " name)
  (os/mkdir name)
  (os/mkdir (string name "/test"))
  (os/mkdir (string name "/" name))
  (os/mkdir (string name "/bin"))
  (spit (string name "/" name "/init.janet") (init-template template-opts))
  (spit (string name "/test/basic.janet") (test-template template-opts))
  (spit (string name "/README.md") (readme-template template-opts))
  (spit (string name "/LICENSE") (license-template template-opts))
  (spit (string name "/CHANGELOG.md") (changelog-template template-opts))
  (cond
    scaffold-spork-free
    (do
      (os/mkdir (string name "/bundle"))
      (spit (string name "/bundle/info.jdn") (bundle-info-template template-opts))
      (spit (string name "/bundle/init.janet") (bundle-init-template template-opts)))
    scaffold-native
    (do
      (os/mkdir (string name "/c"))
      (spit (string name "/c/module.c") (module-c-template template-opts))
      (spit (string name "/test/native.janet") (native-test-template template-opts))
      (spit (string name "/project.janet") (native-project-template template-opts)))
    scaffold-exe
    (do
      (spit (string name "/project.janet") (exe-project-template template-opts)))
    (do
      (spit (string name "/project.janet") (project-template template-opts)))))

(deftemplate enter-shell-template
  :private
    ````
    # . bin/activate
    _OLD_JANET_PATH="$$JANET_PATH";
    _OLD_PATH="$$PATH";
    _OLD_PS1="$$PS1";
    JANET_PATH="$abspath";
    PATH="$$JANET_PATH"/bin:"$$PATH";
    PS1="("$name") $${PS1:-}"
    export _OLD_JANET_PATH;
    export _OLD_PATH;
    export _OLD_PS1;
    export JANET_PATH;
    export PATH;
    export PS1;
    deactivate() {
      PATH="$$_OLD_PATH";
      JANET_PATH="$$_OLD_JANET_PATH";
      PS1="$$_OLD_PS1";
      export JANET_PATH;
      export PATH;
      export PS1;
      unset _OLD_JANET_PATH;
      unset _OLD_PATH;
      unset _OLD_PS1;
      unset -f deactivate;
      export _OLD_JANET_PATH;
      export _OLD_PATH;
      export _OLD_PS1;
      hash -r 2> /dev/null;
    }
    hash -r 2> /dev/null;
    ````)

(deftemplate enter-ps-template
  :private
  ````
  # . bin/activate.ps1
  $$global:_OLD_JANET_PATH=$$env:JANET_PATH
  $$global:_OLD_PATH=$$env:PATH
  $$env:JANET_PATH="$abspath"
  $$env:PATH=$$JANET_PATH + "/bin:" + $$env:PATH
  $$function:old_prompt = $$function:prompt
  function global:prompt {
    Write-Host "($name) " -NoNewline
    & $$function:old_prompt
  }
  function deactivate {
    $$env:PATH=$$global:_OLD_PATH
    $$env:JANET_PATH=$$global:_OLD_JANET_PATH
    Remove-Item function:\deactivate
    $$function:prompt = $$function:old_prompt
    Remove-Item function:\old_prompt
  }
  ````)

(deftemplate enter-cmd-template
  :private
  ````
  @rem bin\activate.bat
  @set _OLD_JANET_PATH=%JANET_PATH%
  @set _OLD_PATH=%PATH%
  @set _OLD_PROMPT=%PROMPT%
  @set JANET_PATH=$abspath
  @set PATH=%JANET_PATH%\bin;%PATH%
  @set PROMPT=($path) %PROMPT%
  ````)

(deftemplate exit-cmd-template
  :private
  ````
  @rem bin\deactivate.bat
  @set JANET_PATH=%_OLD_JANET_PATH%
  @set PATH=%_OLD_PATH%
  @set PROMPT=%_OLD_PROMPT%
  @set _OLD_JANET_PATH=%PATH%
  @set _OLD_PATH=%PATH%
  @set _OLD_PROMPT=%PROMPT%
  ````)

(defn scaffold-pm-shell
  "Generate a pm shell with configuration already setup."
  [path]
  (os/mkdir path)
  (os/mkdir (path/join path "bin"))
  (os/mkdir (path/join path "man"))
  (def opts {:path path :abspath (path/abspath path) :name (path/basename path)})
  (spit (path/join path "bin" "activate") (enter-shell-template opts))
  (spit (path/join path "bin" "activate.ps1") (enter-ps-template opts))
  (spit (path/join path "bin" "activate.bat") (enter-cmd-template opts))
  (spit (path/join path "bin" "deactivate.bat") (exit-cmd-template opts))
  (print "created project shell environment at " path)
  (print "(PowerShell) run `. " path "/bin/activate.ps1` to enter the new environment, then `deactivate` to exit.")
  (print "(CMD)        run `" path "\\bin\\activate` to enter the new environment, then `deactivate` to exit.")
  (print "(Unix sh)    run `. " path "/bin/activate` to enter the new environment, then `deactivate` to exit."))
