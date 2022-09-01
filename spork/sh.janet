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

(defn create-dirs
  "Create all directories in path specified as string including itself."
  [dir-path]
  (def dirs @[])
  (each part (path/parts dir-path)
    (array/push dirs part)
    (def path (path/join ;dirs))
    (if (not (os/stat path))
        (os/mkdir path))))

(defmacro with-file
  "Create and open a file, creating all the directories leading to the file if
  they do not exist, apply the given body on the file resource, and then close
  the file."
  [[binding path mode] & body]
  ~(do
    (def parent-path (,path/dirname ,path))
    (when (and (not (,exists? ,path)) (not (,exists? parent-path)))
      (,create-directories parent-path))
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
