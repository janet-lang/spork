###
### Shell utilties for Janet.
### sh.janet
###

(import ./path)

(defn exec-slurp
   "Read stdout of subprocess and return it trimmed in a string."
   [& args]
   (def proc (os/spawn args :px {:out :pipe}))
   (def out (get proc :out))
   (def buf @"")
   (ev/gather
     (:read out :all buf)
     (:wait proc))
   (string/trimr buf))

(defn rm
  "Remove a directory and all sub directories recursively."
  [path]
  (case (os/lstat path :mode)
    :directory (do
      (each subpath (os/dir path)
        (rm (path/join path subpath)))
      (os/rmdir path))
    nil nil # do nothing if file does not exist
    # Default, try to remove
    (os/rm path)))

(defn exists?
  "Check if the given file or directory exists. (Follows symlinks)"
  [path]
  (not (nil? (os/stat path))))

(defn scan-directory
  "Scan a directory recursively, applying the given function on all files and
  directories in a depth-first manner. This function has no effect if the
  directory does not exist."
  [dir func]
  (def names (map (fn [name] (path/join dir name))
    (try (os/dir dir) ([err] @[]))))
  (defn filter-names [mode]
    (filter
      (fn [name] (= mode (os/stat name :mode)))
      names))
  (def files (filter-names :file))
  (def dirs (filter-names :directory))
  (each dir dirs
    (scan-directory dir func))
  (each file files
    (func file))
  (each dir dirs
    (func dir)))

(defn list-all-files
  "List the files in the given directory recursively. Return the paths to all
  files found, relative to the current working directory if the given path is a
  relative path, or as an absolute path otherwise."
  [dir]
  (def files @[])
  (scan-directory dir (fn [file]
    (when (= :file (os/stat file :mode))
      (array/push files file))))
  files)

(defn create-dirs
  "Create all directories in path specified as string including itself."
  [dir-path]
  (def dirs @[])
  (each part (path/parts dir-path)
    (array/push dirs part)
    (let [path (path/join ;dirs)]
         (if (not (os/lstat path))
             (os/mkdir path)))))

(defmacro with-file
  "Create and open a file, creating all the directories leading to the file if
  they do not exist, apply the given body on the file resource, and then close
  the file."
  [[binding path mode] & body]
  ~(do
    (def parent-path (,path/dirname ,path))
    (when (and (not (,exists? ,path)) (not (,exists? parent-path)))
      (,create-dirs parent-path))
    (def ,binding (file/open ,path ,mode))
    ,(apply defer [:close binding] body)))

(defn copy-file
  "Copy a file from source to destination. Creates all directories in the path
  to the destination file if they do not exist."
  [src-path dst-path]
  (def buf-size 4096)
  (def buf (buffer/new buf-size))
  (with [src (file/open src-path :rb)]
    (with-file [dst dst-path :wb]
      (while (def bytes (file/read src buf-size buf))
        (file/write dst bytes)
        (buffer/clear buf)))))
