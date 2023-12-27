###
### Shell utilties for Janet.
### sh.janet
###

(import ./path)

(defn devnull
  "get the /dev/null equivalent of the current platform as an open file"
  []
  (os/open (if (= :windows (os/which)) "NUL" "/dev/null") :rw))

(defn exec
  "Execute command specified by args returning its exit code"
  [& args]
  (os/execute args :p))

(defn exec-fail
  "Execute command specified by args, fails when command exits with non-zero exit code"
  [& args]
  (os/execute args :px))

(defn exec-slurp
   "Read stdout of command specified by args and return it trimmed in a string."
   [& args]
   (def proc (os/spawn args :px {:out :pipe}))
   (def out (get proc :out))
   (def buf @"")
   (ev/gather
     (:read out :all buf)
     (:wait proc))
   (string/trimr buf))

(defn exec-slurp-all
   `Read stdout and stderr of subprocess and return it trimmed in a struct with :err and :out containing the output as string.
   This will also return the exit code under the :status key.`
   [& args]
   (def proc (os/spawn args :p {:out :pipe :err :pipe}))
   (def out (get proc :out))
   (def err (get proc :err))
   (def out-buf @"")
   (def err-buf @"")
   (var status 0)
   (ev/gather
     (:read out :all out-buf)
     (:read err :all err-buf)
     (set status (:wait proc)))
   {:err (string/trimr err-buf)
    :out (string/trimr out-buf)
    :status status})

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
  (not= nil (os/stat path)))

(defn scan-directory
  "Scan a directory recursively, applying the given function on all files and
  directories in a depth-first manner. This function has no effect if the
  directory does not exist."
  [dir func]
  (each name (try (os/dir dir) ([_] @[]))
    (def fullpath (path/join dir name))
    (case (os/stat fullpath :mode)
      :file (func fullpath)
      :directory (do
                   (scan-directory fullpath func)
                   (func fullpath)))))

(defn list-all-files
  "List the files in the given directory recursively. Return the paths to all
  files found, relative to the current working directory if the given path is a
  relative path, or as an absolute path otherwise."
  [dir &opt into]
  (default into @[])
  (each name (try (os/dir dir) ([_] @[]))
    (def fullpath (path/join dir name))
    (case (os/stat fullpath :mode)
      :file (array/push into fullpath)
      :directory (list-all-files fullpath into)))
  into)

(defn create-dirs
  "Create all directories in path specified as string including itself."
  [dir-path]
  (def dirs @[])
  (each part (path/parts dir-path)
    (array/push dirs part)
    (let [path (path/join ;dirs)]
         (if-not (os/lstat path)
             (os/mkdir path)))))

(defn make-new-file
  "Create and open a file, creating all the directories leading to the file if
  they do not exist, and return it. By default, open as a writable file (mode is `:w`)."
  [file-path &opt mode]
  (default mode :w)
  (let [parent-path (path/dirname file-path)]
    (when (and (not (exists? file-path))
               (not (exists? parent-path)))
      (create-dirs parent-path)))
  (file/open file-path mode))

(defn copy-file
  "Copy a file from source to destination. Creates all directories in the path
  to the destination file if they do not exist."
  [src-path dst-path]
  (def buf-size 4096)
  (def buf (buffer/new buf-size))
  (with [src (file/open src-path :rb)]
    (with [dst (make-new-file dst-path :wb)]
      (while (def bytes (file/read src buf-size buf))
        (file/write dst bytes)
        (buffer/clear buf)))))

(defn copy
  `Copy a file or directory recursively from one location to another.
  Expects input to be unix style paths`
  [src dest]
  (print "copying " src " to " dest "...")
  (if (= :windows (os/which))
    (let [end (last (path/posix/parts src))
          isdir (= (os/stat src :mode) :directory)]
      (os/shell (string "C:\\Windows\\System32\\xcopy.exe"
                        " "
                        (path/win32/join ;(path/posix/parts src))
                        (path/win32/join ;(if isdir [;(path/posix/parts dest) end] (path/posix/parts dest)))
                        "/y "
                        "/s "
                        "/e "
                        "/i ")))
    (os/execute ["cp" "-rf" src dest] :p)))

(def- shlex-grammar (peg/compile ~{
  :ws (set " \t\r\n")
  :escape (* "\\" (capture 1))
  :dq-string (accumulate (* "\"" (any (+ :escape (if-not "\"" (capture 1)))) "\""))
  :sq-string (accumulate (* "'" (any (if-not "'" (capture 1))) "'"))
  :token-char (+ :escape (* (not :ws) (capture 1)))
  :token (accumulate (some :token-char))
  :value (* (any (+ :ws)) (+ :dq-string :sq-string :token) (any :ws))
  :main (any :value)
}))

(defn split
  "Split a string into 'sh like' tokens, returns
   nil if unable to parse the string."
  [s]
  (peg/match shlex-grammar s))

(defn- shell-quote
  [arg]
  (def buf (buffer/new (* (length arg) 2)))
  (buffer/push-string buf "'")
  (each c arg
    (if (= c (chr "'"))
      (buffer/push-string buf "'\\''")
      (buffer/push-byte buf c)))
  (buffer/push-string buf "'")
  (string buf))

(defn escape
  "Output a string with all arguments correctly quoted"
  [& args]
  (string/join (map shell-quote args) " "))
