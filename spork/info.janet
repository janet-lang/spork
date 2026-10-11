(import ./path)

(defn- bundle-dir
  [&opt bundle-name]
  (def s path/sep)
  (string (os/realpath (dyn *syspath*)) s "bundle" (if bundle-name s) bundle-name))

(defn- get-pointer-shift [bits]
  (def ptr-bits (band bits (bnot 0x3)))
  (var res nil)
  (for n 1 28
    (when (= ptr-bits (blshift 0x4 n))
      (set res n)
      (break)))
  res)

(defn- get-config-bits []
  (let [bits janet/config-bits
        current @[]]
    (unless (zero? (band bits 0x1))
      (array/push current "nanbox"))
    (unless (zero? (band bits 0x2))
      (array/push current "single-threaded"))
    (when-let [shift (get-pointer-shift bits)]
      (array/push current (string "pointer-shift=" shift)))
    (string/join current ", ")))

(defn- print-bundle-info [b]
  (printf "  %s%s%s"
          (b :name)
          (if-let [ver (b :version)] (string/format " (%s)" ver) "")
          (if-let [tag (b :tag)] (string/format " [%s]" tag) "")))

(defn print-info []
  (print "Janet:")
  (print "  version:     " janet/version "-" janet/build)
  (print "  platform:    " (os/which) "/" (os/arch) "/" (os/compiler))
  (print "  syspath:     " (dyn :syspath))
  (print "  config bits: " (get-config-bits))
  # the rest won't work on a reduced build
  (compif (dyn 'bundle/list)
    (do
      (def s path/sep)
      (def blist
        (try
          (when-let [d (bundle-dir)
                     _ (= :directory (os/stat d :mode))]
            (map (fn [x]
                   (try (do
                          (def manifest (bundle/manifest x))
                          (def ver (get manifest :version (get-in manifest [:info :version])))
                          (def tag (get manifest :tag (get-in manifest [:info :tag])))
                          {:name x :version ver :tag tag})
                     ([_] {:name x :tag "unable to read manifest"})))
                 (sort (os/dir d))))
          ([_])))
      (print "Installed bundles:")
      (if (and blist (not (empty? blist)))
        (each item blist (print-bundle-info item))
        (print "  None"))
      # handle JPM-installed bundles
      (def jpm-manifest-dir (string (dyn *syspath*) s ".manifests"))
      (def jpmlist
        (when (= :directory (os/stat jpm-manifest-dir :mode))
          (map (fn [x]
                 (when (string/has-suffix? ".jdn" x)
                   (def name (string/slice x 0 -5))
                   (try (do
                          (def manifest (-?> (string jpm-manifest-dir s x) slurp parse))
                          (def ver (get manifest :version (get-in manifest [:info :version])))
                          (def tag (get manifest :tag (get-in manifest [:info :tag])))
                          {:name name :version ver :tag tag})
                     ([_] {:name name :tag "unable to read manifest"}))))
               (sort (os/dir jpm-manifest-dir)))))
      (when (and jpmlist (not (empty? jpmlist)))
        (print "JPM-installed bundles:")
        (each item jpmlist (print-bundle-info item)))
      (print "Environment:")
      (def env (os/environ))
      (eachp [name val] env
        (when (string/has-prefix? "JANET_" name)
          (printf "  %s: %s" name val)))
      (when-let [nc (get env "NO_COLOR")] (printf "  NO_COLOR: %s" nc))
      (print))))
