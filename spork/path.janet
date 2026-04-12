### path.janet
###
### A library for path manipulation.
###
### Copyright 2019 © Calvin Rose

#
# Common
#

(def- ext-peg
  (peg/compile ~{:back (> -1 (+ (* ($) (set "\\/.")) :back))
                 :main :back}))

(defn ext
  "Get the file extension for a path."
  [path]
  (if-let [m (peg/match ext-peg path (length path))]
    (let [i (m 0)]
      (if (= (path i) 46)
        (string/slice path (m 0))))))

(defn- capture-lead
  [& xs]
  [:lead (xs 0)])

#
# Posix
#

(defn posix/abspath?
  "Check if a path is absolute."
  [path]
  (string/has-prefix? "/" path))

(def posix/ext "Get the file extension for a path." ext)

(def posix/sep "Platform separator" "/")

(def posix/delim "Platform delimiter" ":")

(def- posix/last-sep-peg
  (peg/compile '{:back (> -1 (+ (* "/" ($)) :back))
                 :main (+ :back (constant 0))}))

(defn posix/basename
  "Gets the base file name of a path."
  [path]
  (if-let [m (peg/match posix/last-sep-peg path (length path))]
    (let [[p] m]
      (string/slice path p))
    path))

(defn posix/dirname
  "Gets the directory name of a path."
  [path]
  (if-let [m (peg/match posix/last-sep-peg path (length path))]
    (let [[p] m]
      (if (zero? p) "./" (string/slice path 0 p)))
    path))

(defn posix/parent
  "Gets the parent directory name of a path."
  [path]
  (if-let [m (peg/match posix/last-sep-peg path (length path))]
    (let [[p] m]
      (cond (zero? p) ""
        (and (= p 1) (= (string/slice path 0 1) "/")) "/"
        true (string/slice path 0 (- p 1))))
    path))

(defn posix/parts
  "Split a path into its parts."
  [path]
  (string/split "/" path))

(def- posix-normalize-peg
  (peg/compile
    ~{:span (some (if-not "/" 1))
      :sep (some "/")
      :main (* (? (* (/ '"/" ,capture-lead) (any "/")))
               (? ':span)
               (any (* :sep ':span))
               (? (* :sep (constant ""))))}))

(defn posix/normalize
  "Normalize a path. This removes . and .. in the
   path, as well as empty path elements."
  [path]
  (def accum @[])
  (def parts (peg/match posix-normalize-peg path))
  (var seen 0)
  (var lead nil)
  (each x parts
    (match x
      [:lead what] (set lead what)
      "." nil
      ".." (if (and (nil? lead) (= 0 seen))
             (array/push accum x)
             (do
               (when (< 0 seen) (-- seen))
               (array/pop accum)))
      (do (++ seen) (array/push accum x))))
  (def ret (string (or lead "") (string/join accum "/")))
  (if (= "" ret) "." ret))

(defn posix/join
  "Join path elements together."
  [& els]
  (posix/normalize (string/join els "/")))

(defn posix/abspath
  "Coerce a path to be absolute."
  [path]
  (if (posix/abspath? path)
    (posix/normalize path)
    (posix/join (or (dyn :path-cwd) (os/cwd)) path)))

(defn posix/relpath
  "Get the relative path between two subpaths."
  [source target]
  (def source-parts
    (filter next (posix/parts (posix/abspath source))))
  (def target-parts
    (filter next (posix/parts (posix/abspath target))))
  (def same-parts
    (length (take-until identity (map not= source-parts target-parts))))
  (def up-walk (array/new-filled (- (length source-parts) same-parts) ".."))
  (def down-walk (tuple/slice target-parts same-parts))
  (posix/join ;up-walk ;down-walk))

###########################################################################
#
# Windows
#
###########################################################################

(def- win-prefix-peg
  (peg/compile
    ~{:drive (* (range "AZ" "az") `:` (any (+ `\` `/`)) ($))
      :dos-unc (* `\\` (+ "." "?") `\UNC\`
                  (some (if-not `\` 1)) `\` (some (if-not `\` 1)) (any `\`) ($))
      :dos (* `\\` (+ "." "?") `\` (some (if-not `\` 1)) (any `\`) ($))
      :unc (* `\\` (some (if-not `\` 1)) `\` (some (if-not `\` 1)) (any `\`) ($))
      :main (+ :drive :dos-unc :dos :unc)}))

(defn win32/abspath?
  "Check if a path is absolute."
  [path]
  (not (nil? (peg/match win-prefix-peg path))))

(defn- win32-path-prefix [path]
  (if-let [m (peg/match win-prefix-peg path)]
    (let [[p] m]
      p)
    0))

# need to use a peg to allow for mixed `\` and `/` in the
# same Windows path.
(def- all-sep-peg
  (peg/compile ~(any (+ (some (* ($) (+ `\` `/`) 1)) 1))))

(defn- sep-split
  "Split string based on separator peg"
  [path]
  (let [locs (peg/match all-sep-peg path)
        parts @[]]
    (var start 0)
    (each l locs
      (array/concat parts (string/slice path start l))
      (set start (inc l)))
    (when (< start (length path))
      (array/concat parts (string/slice path start)))
    (filter |(> (length $) 0) parts)))

(def win32/ext "Get the file extension for a path." ext)

(def win32/sep "Platform separator" `\`)

(def win32/delim "Platform delimiter" ";")

(def- win32/last-sep-peg
  (peg/compile '{:back (> -1 (+ (* (set `\/`) ($)) :back))
                 :main (+ :back (constant 0))}))

(defn win32/basename
  "Gets the base file name of a path."
  [path]
  (if-let [m (peg/match win32/last-sep-peg path (length path))]
    (let [[p] m
          prefix-end (win32-path-prefix path)]
      (if (zero? prefix-end)
        (string/slice path p)
        (if (> prefix-end p)
          "" # last separator is inside the prefix, so basename is blank
          (string/slice path p))))
    path))

(defn win32/dirname
  "Gets the directory name of a path."
  [path]
  (if-let [m (peg/match win32/last-sep-peg path (length path))]
    (let [[p] m
          prefix-end (win32-path-prefix path)]
      (if (zero? p)
        (if (> prefix-end 0) path `.\`)
        (if (> prefix-end p) path (string/slice path 0 p))))
    path))

(defn win32/parent
  "Gets the parent directory name of a path."
  [path]
  (if-let [m (peg/match win32/last-sep-peg path (length path))]
    (let [[p] m
          prefix-end (win32-path-prefix path)]
      (cond (and (zero? prefix-end) (zero? p))
        ""
        (and (zero? prefix-end) (not (zero? p)))
        (string/slice path 0 (dec p))
        (and (not (zero? prefix-end)) (zero? p))
        path
        true
        (if (= prefix-end (length path))
          path
          (string/slice path 0 (if (= p prefix-end) p (dec p))))))
    path))

# if there is a prefix (drive letter or unc location),
# add it to output then split the rest else just split the whole thing
(defn win32/parts
  "Split a path into its parts."
  [path]
  (let [start (win32-path-prefix path)
        rest-path (string/slice path start)]
    (if (zero? start)
      (sep-split path)
      (array/concat @[(string/slice path 0 start)] (sep-split rest-path)))))

(def- win32-normalize-peg
  (peg/compile
    ~{:span (some (if-not (set `\/`) 1))
      :sep (some (set `\/`))
      :main (* (? (* (/ '(+ (* `\\` (some (if-not `\` 1)) `\`)
                            (* (? (* (range "AZ" "az") `:`)) `\`))
                        ,capture-lead)
                     (any (set `\/`))))
               (? ':span)
               (any (* :sep ':span))
               (? (* :sep (constant ""))))}))

(defn win32/normalize
  "Normalize a path. This removes . and .. in the
   path, as well as empty path elements."
  [path]
  (def accum @[])
  (def parts (peg/match win32-normalize-peg path))
  (var seen 0)
  (var lead nil)
  (each x parts
    (match x
      [:lead what] (set lead what)
      "." nil
      ".." (if (and (nil? lead) (= 0 seen))
             (array/push accum x)
             (do
               (when (< 0 seen) (-- seen))
               (array/pop accum)))
      (do (++ seen) (array/push accum x))))
  (def ret (string (or lead "") (string/join accum `\`)))
  (if (= "" ret) "." ret))

(defn win32/join
  "Join path elements together."
  [& els]
  (win32/normalize (string/join els `\`)))

(defn win32/abspath
  "Coerce a path to be absolute."
  [path]
  (if (win32/abspath? path)
    (win32/normalize path)
    (win32/join (or (dyn :path-cwd) (os/cwd)) path)))

(defn win32/relpath
  "Get the relative path between two subpaths."
  [source target]
  (def source-parts
    (filter next (win32/parts (win32/abspath source))))
  (def target-parts
    (filter next (win32/parts (win32/abspath target))))
  (def same-parts
    (length (take-until identity (map not= source-parts target-parts))))
  (def up-walk (array/new-filled (- (length source-parts) same-parts) ".."))
  (def down-walk (tuple/slice target-parts same-parts))
  (win32/join ;up-walk ;down-walk))

#
# Specialize for current OS
#

(def- posix? (delay (let [tos (os/which)]
                      (and (not= :windows tos) (not= :mingw tos)))))

(def sep
  "Platform separator"
  (if (posix?) posix/sep win32/sep))

(def delim
  "Platform delimiter"
  (if (posix?) posix/delim win32/delim))

(def basename
  "Gets the base file name of a path."
  (if (posix?) posix/basename win32/basename))

(def dirname
  "Gets the directory name of a path."
  (if (posix?) posix/dirname win32/dirname))

(def parent
  "Gets the parent directory name of a path."
  (if (posix?) posix/parent win32/parent))

(def abspath?
  "Check if a path is absolute."
  (if (posix?) posix/abspath? win32/abspath?))

(def abspath
  "Coerce a path to be absolute."
  (if (posix?) posix/abspath win32/abspath))

(def parts
  "Split a path into its parts."
  (if (posix?) posix/parts win32/parts))

(def normalize
  "Normalize a path. This removes . and .. in the
 as well as empty path elements."
  (if (posix?) posix/normalize win32/normalize))

(def join
  "Join path elements together."
  (if (posix?) posix/join win32/join))

(def relpath
  "Get the relative path between two subpaths."
  (if (posix?) posix/relpath win32/relpath))
